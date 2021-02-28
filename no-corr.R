# run model without state correlation

dat_list <- list(
    state_id = state_ids[polls_2016$state, ],
    n = polls_2016$n_two_parties,
    y = polls_2016$clinton_observations,
    prior_mean = prior_mean,
    prior_std = sqrt(diag(prior_covariance))
)

m.no.corr <- ulam(
    alist(
        y ~ dbinom( n, p ),
        logit(p) <- alpha[state_id],
        alpha[state_id] ~ dnorm(prior_mean, prior_std)
    ),
    data=dat_list,
    chains=4,
    cores=4,
    cmdstan = TRUE,
    log_lik=TRUE
)

#stancode(m.no.corr)

post_no_corr <- extract.samples(m.no.corr, n=3000)
prior_no_corr <- extract.prior(m.no.corr, n=3000, cmdstan = TRUE)

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

prior_intervals <- mean_low_high(prior_no_corr$alpha, states = state_data$State, id = "prior")
posterior_intervals <- mean_low_high(post_no_corr$alpha, states = state_data$State, id = "posterior")
alpha_intervals <- rbind(prior_intervals, posterior_intervals)
