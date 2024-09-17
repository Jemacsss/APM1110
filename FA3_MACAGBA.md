FA3_MACAGBA
================
2024-09-13

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.3.3

``` r
library(moments)

testscores <- c(88, 45, 53, 86, 33, 86, 85, 30, 89, 53, 41, 96, 56, 38, 62, 71, 51, 86, 68, 29, 28, 47, 33, 37, 25, 36, 33, 94, 73, 46, 42, 34, 79, 72, 88, 99, 82, 62, 57, 42, 28, 55, 67, 62, 60, 96, 61, 57, 75, 93, 34, 75, 53, 32, 28, 73, 51, 69, 91, 35)

modef <- function(value){
  uniqvalue <- unique(value)
  uniqvalue[which.max(tabulate(match(value,uniqvalue)))]
}
```

``` r
valid <- sum(!is.na(testscores))


modev <- modef(testscores)


medianv <- median(testscores)


meanv <- mean(testscores)


std_dev <- sd(testscores)

variance <- var(testscores)

x <- skewness(testscores)


skewnesserror <- length(testscores)

stderrorskewness <- sqrt(6/x)


kurtosisv <- kurtosis(testscores)


stderrorkurtosis <- sqrt(24/x)


percentile <- quantile(testscores, probs = c(0.25,0.5,0.75,0.9,0.95))

minimumv <- min(testscores)

maximumv <- max(testscores)
```

``` r
table <- data.frame(
  
Measure = c("Valid", "Mode", "Median", "Mean", "Standard Deviation", "Variance", "Skewness", "Standard Error of Skewness", "Kurtosis", "Standard Error of Kurtosis", "Minimum", "Maximum", "25th Percentile", "50th Percentile", "75th Percentile", "90th Percentile", "95th Percentile"),

Value = c(valid, modev, medianv, meanv, std_dev, variance,skewnesserror, stderrorskewness, kurtosisv, stderrorkurtosis, minimumv, maximumv, percentile[1], percentile [2], percentile [3], percentile[4], percentile[5])
)
print(table)
```

    ##                       Measure      Value
    ## 1                       Valid  60.000000
    ## 2                        Mode  53.000000
    ## 3                      Median  57.000000
    ## 4                        Mean  59.166667
    ## 5          Standard Deviation  22.210981
    ## 6                    Variance 493.327684
    ## 7                    Skewness  60.000000
    ## 8  Standard Error of Skewness   6.067745
    ## 9                    Kurtosis   1.758792
    ## 10 Standard Error of Kurtosis  12.135490
    ## 11                    Minimum  25.000000
    ## 12                    Maximum  99.000000
    ## 13            25th Percentile  37.750000
    ## 14            50th Percentile  57.000000
    ## 15            75th Percentile  76.000000
    ## 16            90th Percentile  89.200000
    ## 17            95th Percentile  94.100000