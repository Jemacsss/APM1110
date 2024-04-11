FA8
================
2024-04-11

``` r
#1
mean <- 200
variance <- 256
standard_deviation <- sqrt(variance)

#a) probability will exceed 224
exceed_224 <- 1 - pnorm(224, mean, standard_deviation)

#b) probability between 186-224
between_186_224 <- pnorm(224, mean, standard_deviation) - pnorm(186, mean, standard_deviation)

#c) voltage below 25%
voltage_25_percentile <- qnorm(0.25, mean, standard_deviation)

#d) less than 240 greater than 210
less_240_greater_210 <- (pnorm(240, mean, standard_deviation) - pnorm(210, mean, standard_deviation)) / (1 - pnorm(210, mean, standard_deviation))

#e) estimate interquartile range
quartile_1 <- qnorm(0.25, mean, standard_deviation)
quartile_3 <- qnorm(0.75, mean, standard_deviation)
interquartile_range <- quartile_3 - quartile_1

#f) less than 220 greater 210
less_220_greater_210 <- (pnorm(220, mean, standard_deviation) - pnorm(210, mean, standard_deviation)) / (1 - pnorm(210, mean, standard_deviation))

#g) given 200 greater than 220
greater_220_greater_200 <- (1 - pnorm(220, mean, standard_deviation)) / (1 - pnorm(200, mean, standard_deviation))

cat("a) Probability that the signal will exceed 224 µV:", exceed_224, "\n")
```

    ## a) Probability that the signal will exceed 224 µV: 0.0668072

``` r
cat("b) Probability that it will be between 186 and 224 µV:", between_186_224, "\n")
```

    ## b) Probability that it will be between 186 and 224 µV: 0.7424058

``` r
cat("c) Micro voltage below which 25% of the signals will be:", voltage_25_percentile, "\n")
```

    ## c) Micro voltage below which 25% of the signals will be: 189.2082

``` r
cat("d) Probability that the signal will be less than 240 µV, given that it is larger than 210 µV:", less_240_greater_210, "\n")
```

    ## d) Probability that the signal will be less than 240 µV, given that it is larger than 210 µV: 0.9766541

``` r
cat("e) Estimate of the interquartile range:", interquartile_range, "\n")
```

    ## e) Estimate of the interquartile range: 21.58367

``` r
cat("f) Probability that the signal will be less than 220 µV, given that it is larger than 210 µV:", less_220_greater_210, "\n")
```

    ## f) Probability that the signal will be less than 220 µV, given that it is larger than 210 µV: 0.6027988

``` r
cat("g) Probability that a received signal greater than 200 µV is actually greater than 220 µV:", greater_220_greater_200, "\n")
```

    ## g) Probability that a received signal greater than 200 µV is actually greater than 220 µV: 0.2112995

``` r
#2
avg_downtime <- 25
variance <- 144
sd <- sqrt(variance)


lowerbound <- qnorm(0.025, avg_downtime, sd)
upperbound <- qnorm(0.975, avg_downtime, sd)


boundabove10 <- qnorm(0.90, avg_downtime, sd)


cat("a) Bounds which will include 95% of the downtime of all the customers:", lowerbound, "to", upperbound, "\n")
```

    ## a) Bounds which will include 95% of the downtime of all the customers: 1.480432 to 48.51957

``` r
cat("b) Bound above which 10% of the downtime is included:", boundabove10, "\n")
```

    ## b) Bound above which 10% of the downtime is included: 40.37862
