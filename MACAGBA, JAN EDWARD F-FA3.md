    transmitted0 <- 0.70
    transmitted1 <- 1 - transmitted0
    correct0 <- 0.95
    correct1 <- 0.75

    # a 1 was received;
    P_1R <- transmitted0 * (1 - correct0) + transmitted1 * correct1
    P_1R

    ## [1] 0.26

    # a 1 was transmitted given that a 1 was received.
    P1_G1R <- (transmitted1 * correct1) / P_1R
    P1_G1R

    ## [1] 0.8653846

    barplot(c(P_1R, P1_G1R) * 100,
            names.arg = c("P(1 received)", "P(1 transmitted | 1 received)"),
            ylab = "Probability Percentage",
            main = "Probability of 1 Being Received and Transmitted",
            col = c("lightblue", "lightgreen"),
            ylim = c(0, max(c(P_1R, P1_G1R) * 100) + 5))

    text(1:2, c(P_1R, P1_G1R) * 100 + 1, 
         labels = round(c(P_1R, P1_G1R) * 100, 2), pos = 3, col = "darkred")

![](FA3markdown_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    Jane <- 0.10
    Amy <- 0.30
    Ava <- 0.60
    ErrorJane <- 0.08
    ErrorAmy <- 0.05
    ErrorAva <- 0.01

    Error_P <- Jane * ErrorJane + Amy * ErrorAmy + Ava * ErrorAva
    Error_P

    ## [1] 0.029

    FE_Jane <- (Jane * ErrorJane) / Error_P
    FE_Amy <- (Amy * ErrorAmy) / Error_P
    FE_Ava <- (Ava * ErrorAva) / Error_P

    FE_Jane

    ## [1] 0.2758621

    FE_Amy

    ## [1] 0.5172414

    FE_Ava

    ## [1] 0.2068966

    barplot(c(FE_Jane, FE_Amy, FE_Ava) * 100,
            names.arg = c("Jane", "Amy", "Ava"),
            ylab = "Probability Percentage",
            main = "Probability of Each Person Writing a Program with an Error",
            col = "lightblue",
            ylim = c(0, max(c(FE_Jane, FE_Amy, FE_Ava) * 100) + 5))

    text(1:3, c(FE_Jane, FE_Amy, FE_Ava) * 100 + 1, 
         labels = round(c(FE_Jane, FE_Amy, FE_Ava) * 100, 2), 
         pos = 3, col = "darkred")

![](FA3markdown_files/figure-markdown_strict/unnamed-chunk-6-1.png)
