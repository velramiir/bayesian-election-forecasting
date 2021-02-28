# wip code measurement error

dat_list <- list(
    state_id = state_ids[polls_2016$state, ],
    n = polls_2016$n_two_parties,
    y_obs = polls_2016$clinton_observations,
    prior_mean = prior_mean,
    prior_covariance = prior_covariance
)

m.binom <- ulam(
    alist(
        vector[1644]:y_obs ~ dnorm( y_true, y_sd[state_id] ),
        vector[50]:y_sd ~ exponential(1),
        vector[1644]:y_true ~ dbinom( n, p ),
        logit(p) <- alpha[state_id] + e,
        vector[50]:alpha ~ dmvnorm(prior_mean, prior_covariance)
    ),
    data=dat_list,
    chains=4,
    cores=4,
    cmdstan = TRUE
)