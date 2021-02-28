# generate plots for reports and presentation

library(dplyr)
library(lubridate)

state_data_with_dc = read.delim("electoral_college_votes.tsv")
state_data = dplyr::filter(state_data_with_dc, State != "DC")

polls_raw = read.table("all_polls.csv", sep=",", header=TRUE, row.names = 1) %>% 
    mutate(year = lubridate::year(end.date)) %>%
    filter(as.integer(year) <= 2016)

states_per_pollster <- polls_raw %>%
    filter(state != "--") %>%
    group_by(pollster) %>%
    summarise(count = n_distinct(state))

barplot(sort(states_per_pollster$count, decreasing=TRUE), ylab = "states_polled", xlab ="pollsters")

polls_each_month <- polls_raw %>%
    filter(year == "2016") %>%
    mutate(month = lubridate::month(end.date)) %>%
    group_by(month) %>%
    summarise(n = n())

days_in_month = c(31,28,31,30,31,30,31,31,30,31,7)

barplot(polls_each_month$n / days_in_month, ylab = "average polls a day", xlab ="month", names.arg = polls_each_month$month)

polling_freq_state <- polls_raw %>%
    filter(state != "--") %>%
    group_by(state) %>%
    summarise(n = n()) %>%
    mutate(freq = n / as.integer(ymd(max(polls_raw$end.date)) - ymd(min(polls_raw$end.date)))) %>%
    arrange(freq, decreasing = FALSE)

barplot(polling_freq_state$freq, ylab = "average polls a day", xlab ="state", names.arg = polling_freq_state$state, las=2)

historical_data = read.table("us_presidential_election_data_historical.tsv", 
                             sep="\t", 
                             header = TRUE)
historical_data = tail(historical_data, nrow(historical_data) - 2)
historical_data = dplyr::select(historical_data, -one_of("X"))
names(historical_data) = sapply(names(historical_data), FUN = function(name) {return(sub("\\.1", ".Republican", name))})
historical_data[] = lapply(historical_data, as.numeric)
historical_data = rbind.data.frame(historical_data[1:8,], historical_data[10:51,]) # filter out dc, because there are no polls in dc
rownames(historical_data) = seq(length=nrow(historical_data))
logits = log(dplyr::select(historical_data, !contains("Republican")) / dplyr::select(historical_data, contains("Republican")))
sample_covariance = cov(t(logits))
sample_variance = diag(sample_covariance)
state_variance = data.frame(state = state_data$State, variance = sample_variance) %>% 
    inner_join(polling_freq_state) %>%
    arrange(freq) %>%
    select(state, variance)

barplot(sqrt(state_variance$variance), ylab = "historical_std", xlab ="state", names.arg = state_variance$state, las=2)

binary_gini_idx <- function(x) {
    dem_prob = sum(x) / length(x)
    rep_prob = 1 - dem_prob
    return(2 * dem_prob * rep_prob)
}

winners = data.frame(logits >= 0)
gini = winners %>%
    mutate(gini = apply(winners, 1, function(x) {
        #v = as.integer(x) + 1
        #lawstat::gini.index(v)$statistic[["Gini Index"]] # gini index seems not right
        binary_gini_idx(x)
    })) %>% 
    bind_cols(state = state_data$State) %>%
    select(gini, state) %>%
    arrange(gini)
barplot(gini$gini, ylab = "historical gini impurity of winner", xlab ="state", names.arg = gini$state, las=2)

swinginess <- function(x) {
    v = as.integer(x) + 1
    sapply(1:length(x), function(n) {
        if (n == length(x)) return(0)
        lawstat::gini.index(v[n:length(x)])$statistic[["Gini Index"]]
    })
}
winners = data.frame(logits >= 0)
swing = data.frame(t(apply(winners, 1, swinginess)))
colnames(swing) = colnames(winners)
swing = swing %>%
    select(!contains("2012")) %>%
    bind_cols(state = state_data$State) %>%
    arrange(state)
#barplot(winners$std, ylab = "historical_gini_index_of_winner", xlab ="state", names.arg = winners$state, las=2)
apply(swing, 1, function(r) plot(r[1:length(r)-1], type="l", sub=r[length(r)], xlab = "year", ylab = "Gini Index"), ylim=c(0, 0.3))
