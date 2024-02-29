    lambda <- 4

    prob_el <- ppois(0.25, lambda)
    prob_el

    ## [1] 0.01831564

    prob_greater <- 1 - ppois(0.5, lambda)
    prob_greater

    ## [1] 0.9816844

    prob_between <- ppois(1, lambda) - ppois(0.25, lambda)
    prob_between

    ## [1] 0.07326256

    jobsub <- 2

    prob_morethan2 <- 1 - ppois(2, jobsub)
    prob_morethan2

    ## [1] 0.3233236

    prob_least_30sec <- 1 - ppois(0,jobsub)
    prob_least_30sec

    ## [1] 0.8646647

    prob_less_30sec <- ppois(0,jobsub)
    prob_less_30sec

    ## [1] 0.1353353

    prob_arrive <- 1 - ppois(0,jobsub)
    prob_arrive

    ## [1] 0.8646647

    visits <- 15/60

    prob_atleast10 <- ppois(9,visits)
    prob_atleast10

    ## [1] 1

    prob_less8 <- ppois(7,visits*60)
    prob_less8

    ## [1] 0.01800219

    prob_visit15 <- 1 - ppois(14,visits)
    prob_visit15

    ## [1] 0

    topquart <- qpois(0.75, visits)
    topquart

    ## [1] 0
