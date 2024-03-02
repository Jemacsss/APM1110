    image_supplied= c(0.15,0.20,0.25,0.40)
    image_relevant= c(0.5,0.60,0.8,0.85)

    p_weighted= image_supplied*image_relevant*100
    p_weighted

    ## [1]  7.5 12.0 20.0 34.0

    cat("The overall percentage of relevant image:",sum(p_weighted),"%")

    ## The overall percentage of relevant image: 73.5 %

    coins <- c("HH","HT","TH","TT")

    E1 <- c("HH","TT")
    E2 <- c("HH","HT")
    E3 <- c("TH","HH")

    prob <- function(event, coins){
      length(event)/length(coins)
    }

    prob_E1 <- prob(E1, coins)
    prob_E2 <- prob(E2, coins)
    prob_E3 <- prob(E3, coins)

    prob_E1E2 <- prob(intersect(E1, E2), coins)
    prob_E1E3 <- prob(intersect(E1, E3), coins)
    prob_E2E2 <- prob(intersect(E2, E3), coins)

    pairwise <- all(
      prob_E1E2 == prob_E1 * prob_E2,
      prob_E1E3 == prob_E1 * prob_E3,
      prob_E2E2 == prob_E2 * prob_E3
    )

    mutual <-prob(intersect(intersect(E1, E2), E3), coins) == prob_E1 * prob_E2 * prob_E3

    cat("Pairwise Independence:", pairwise, "\n")

    ## Pairwise Independence: TRUE

    cat("Mutual Independence:", mutual, "\n")

    ## Mutual Independence: FALSE
