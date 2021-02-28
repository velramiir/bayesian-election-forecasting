library(rethinking)
library(dplyr)
options(mc.cores = parallel::detectCores())

# Data loading and preprocessing

# ec votes and state names
state_data_with_dc = read.delim("electoral_college_votes.tsv")
state_data = dplyr::filter(state_data_with_dc, State != "DC")
state_ids = data.frame(id = seq(1, nrow(state_data)), row.names = state_data$State)
electoral_college_votes = state_data$Electoral.college.votes

# previous elections
historical_data = read.table("us_presidential_election_data_historical.tsv", 
                             sep="\t", 
                             header = TRUE)
historical_data = tail(historical_data, nrow(historical_data) - 2)
historical_data = dplyr::select(historical_data, -one_of("X"))
rownames(historical_data) = seq(length=nrow(historical_data))
names(historical_data) = sapply(names(historical_data), FUN = function(name) {return(sub("\\.1", ".Republican", name))})
historical_data[] = lapply(historical_data, as.numeric)
historical_data = rbind.data.frame(historical_data[1:8,], historical_data[10:51,]) # filter out dc, because there are no polls in dc

# polling data
polls_raw = read.table("all_polls.csv", sep=",", header=TRUE, row.names = 1)
polls_2016 = dplyr::filter(polls_raw, state != "--" & !is.na(number.of.observations))
polls_2016$other = rowSums(polls_2016[,c("other", "undecided", "johnson", "mcmullin")], na.rm=TRUE)
polls_2016 = dplyr::select(polls_2016, "state", "pollster", "number.of.observations", "trump", "clinton", "other")
polls_2016$total = rowSums(polls_2016[,c("trump", "clinton", "other")], na.rm=FALSE)
polls_2016$clinton_observations = round(polls_2016$number.of.observations * (polls_2016$clinton / polls_2016$total))
polls_2016$trump_observations = round(polls_2016$number.of.observations * (polls_2016$trump / polls_2016$total))

polls_2016$n_two_parties = polls_2016$clinton_observations + polls_2016$trump_observations

# 1 = democrats, 0 = republicans
election_winner = function(alpha) {
    democrat_votes = ifelse(alpha > 0.5, electoral_college_votes, 0)
    w = as.numeric(sum(democrat_votes) / sum(electoral_college_votes) > 0.5)
    return(w)
}

electoral_votes <- function(alpha) {
    democrat_votes = ifelse(alpha > 0.5, electoral_college_votes, 0)
    return(sum(democrat_votes))
}

polls_alpha = data.frame(state = polls_2016$state,
                         alpha = log(polls_2016$clinton_observations / polls_2016$trump_observations))

# computing priors
results_2012 = dplyr::select(historical_data, "X2012", "X2012.Republican")
row.names(results_2012) = 1:50
prior_mean = log(results_2012[,1] / results_2012[,2])

logits = log(dplyr::select(historical_data, !contains("Republican")) / dplyr::select(historical_data, contains("Republican")))
sample_covariance = cov(t(logits))
prior_covariance = sample_covariance
diag(prior_covariance) = diag(prior_covariance) + 0.01

# running model

dat_list <- list(
    state_id = state_ids[polls_2016$state, ],
    n = polls_2016$n_two_parties,
    y = polls_2016$clinton_observations,
    prior_mean = prior_mean,
    prior_covariance = prior_covariance
)

m.binom <- ulam(
    alist(
        y ~ dbinom( n, p ),
        logit(p) <- alpha[state_id],
        vector[50]:alpha ~ dmvnorm(prior_mean, prior_covariance)
    ),
    data=dat_list,
    chains=4,
    cores=4,
    cmdstan = TRUE,
    log_lik=TRUE
)

# drawing from distributions

post_corr <- extract.samples(m.binom, n=3000)
prior_corr <- extract.prior(m.binom, n=3000, cmdstan = TRUE)

# taken from TheEconomist/us-potus-model
mean_low_high <- function(draws, states, id){
    tmp <- draws
    draws_df <- data.frame(mean = inv_logit(apply(tmp, MARGIN = 2, mean)),
                           high = inv_logit(apply(tmp, MARGIN = 2, mean) + 1.96 * apply(tmp, MARGIN = 2, sd)), 
                           low  = inv_logit(apply(tmp, MARGIN = 2, mean) - 1.96 * apply(tmp, MARGIN = 2, sd)),
                           state = states, 
                           type = id)
    return(draws_df) 
}

# compute intervals
prior_intervals <- mean_low_high(prior_corr$alpha, states = state_data$State, id = "prior")
posterior_intervals <- mean_low_high(post_corr$alpha, states = state_data$State, id = "posterior")
alpha_intervals <- rbind(prior_intervals, posterior_intervals)

# plot, taken from TheEconomist/us-potus-model
alpha_intervals_plt <- alpha_intervals %>% 
    bind_rows(
        politicaldata::pres_results %>% filter(year == 2016) %>%
            mutate(mean = dem/(dem+rep)) %>%
            select(state,mean) %>%
            mutate(type = 'actual')
    ) %>%
    arrange(mean) %>%
    filter(state %in% c('WY', 'UT', 'KY', 'AR', 'OH', 'FL', 'PA', 'MI', 'NY', 'CA', 'HI', 'VT')) %>%
    ggplot(.) +
    geom_point(aes(y = mean, x = reorder(state, mean), color = type), position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = low, ymax = high, x = state, color = type), width = 0, position = position_dodge(width = 0.5)) +
    coord_flip() +
    theme_bw() +
    ylab("predicted vote share") +
    xlab("state")
alpha_intervals_plt

# plot electoral college vote diagram, taken from TheEconomist/us-potus-model
final_evs = data.frame(draw=1:nrow(post_corr$alpha),
                       dem_ev=apply(post_corr$alpha, MARGIN = 1, FUN = function(row) {sum((inv_logit(row) > 0.5) * state_data$Electoral.college.votes)})
                       )

ev.gg <- ggplot(final_evs,aes(x=dem_ev,
                              fill=ifelse(dem_ev>=270,'Democratic','Republican'))) +
    geom_vline(xintercept = 270) +
    geom_histogram(binwidth=1) +
    theme_minimal() +
    theme(legend.position = 'top',
          panel.grid.minor = element_blank()) +
    scale_fill_manual(name='Electoral College winner',values=c('Democratic'='#3A4EB1','Republican'='#E40A04')) +
    labs(x='Democratic electoral college votes',
         subtitle=sprintf("p(dem win) = %s",round(mean(final_evs$dem_ev>=270),2)) )
ev.gg
