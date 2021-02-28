# generate interval plots for report
# add likelihood for baseline model

############################################

library(dplyr)
library(lubridate)

state_data_with_dc = read.delim("electoral_college_votes.tsv")
state_data = dplyr::filter(state_data_with_dc, State != "DC")

polls_in = read.table("all_polls.csv", sep=",", header=TRUE, row.names = 1) %>% 
    mutate(year = lubridate::year(end.date)) %>%
    filter(as.integer(year) == 2016)

polls_alpha = polls_in %>% 
    filter(state != "--") %>% 
    mutate(alpha = log(clinton/trump)) %>% 
    select(state, alpha) %>% 
    group_by(state)

poll_intervals = data.frame()
for(s in state_data$State) {
    state_polls = polls_alpha %>% filter(state == s)
    state_mean = inv_logit(mean(state_polls$alpha))
    state_high = inv_logit(mean(state_polls$alpha) + 1.96 * sd(state_polls$alpha))
    state_low = inv_logit(mean(state_polls$alpha) - 1.96 * sd(state_polls$alpha))
    poll_intervals = poll_intervals %>% bind_rows(list(mean=state_mean, high=state_high, low=state_low, state=s, type="polls"))
}

# taken from TheEconomist/us-potus-model
poll_intervals_plt <- poll_intervals %>% 
    bind_rows(
        alpha_intervals
    ) %>%
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
    #geom_hline(yintercept=mean_line, linetype='twodash') +
    coord_flip() +
    theme_bw() +
    ylab("predicted democrat vote share") +
    xlab("state")
poll_intervals_plt
