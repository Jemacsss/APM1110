FA6_Macagba
================
2024-09-20

``` r
grades <- c("90-100","80-89","70-79","60-69","50-59","40-49","30-39")
students <- c(9,32,43,21,11,3,1)
totalstudents <- c(120)

grade_distribution <- data.frame(Grade = grades, totalstudents = students)
grade_distribution$Cumulative_Freq <- cumsum(grade_distribution$totalstudents)

Q1pos <- totalstudents* 0.25
Q2pos <- totalstudents* 0.50
Q3pos <- totalstudents* 0.75

find_quartile <- function(postion, Cumulative_Freq){
  return(grade_distribution$Grade[min(which(Cumulative_Freq >= postion))])
}

Q1 <- find_quartile(Q1pos, grade_distribution$Cumulative_Freq)
Q2 <- find_quartile(Q2pos, grade_distribution$Cumulative_Freq)
Q3 <- find_quartile(Q3pos, grade_distribution$Cumulative_Freq)
print("Grade Distribution Table: ")
```

    ## [1] "Grade Distribution Table: "

``` r
print(grade_distribution)
```

    ##    Grade totalstudents Cumulative_Freq
    ## 1 90-100             9               9
    ## 2  80-89            32              41
    ## 3  70-79            43              84
    ## 4  60-69            21             105
    ## 5  50-59            11             116
    ## 6  40-49             3             119
    ## 7  30-39             1             120

``` r
cat("Q1 Grade Interval: ", Q1)
```

    ## Q1 Grade Interval:  80-89

``` r
cat("Q1 Grade Interval: ", Q2)
```

    ## Q1 Grade Interval:  70-79

``` r
cat("Q1 Grade Interval: ", Q3)
```

    ## Q1 Grade Interval:  60-69

``` r
meanstat <- 78
sdstat <- 8.0

meanalgebra <- 73
sdalgebra <- 7.6

absolute_dispersion <- data.frame(subjects = 
                        c("Statistics","Algebra"),
                        SD = c(sdstat,sdalgebra))
relative_dispersion <- data.frame(subjects = 
                        c("Statistics, Algerbra"),
                        cv = c((sdstat/meanstat)*100,
                               (sdalgebra/meanalgebra)*100))

print("Absolute Dispersion: ")
```

    ## [1] "Absolute Dispersion: "

``` r
print(absolute_dispersion)
```

    ##     subjects  SD
    ## 1 Statistics 8.0
    ## 2    Algebra 7.6

``` r
print("Relative Dispersion: ")
```

    ## [1] "Relative Dispersion: "

``` r
print(relative_dispersion)
```

    ##               subjects       cv
    ## 1 Statistics, Algerbra 10.25641
    ## 2 Statistics, Algerbra 10.41096

``` r
data <- c(6, 2, 8, 7, 5)


meandata <- mean(data)
sddata <- sd(data)

zscores <- (data - meandata) / sddata

mean_z <- mean(zscores)
sd_z <- sd(zscores)

cat("Mean of original data:", meandata, "\n")
```

    ## Mean of original data: 5.6

``` r
cat("Standard Deviation of original data:", sddata, "\n")
```

    ## Standard Deviation of original data: 2.302173

``` r
cat("Z-scores:", zscores, "\n")
```

    ## Z-scores: 0.1737489 -1.56374 1.042493 0.6081211 -0.2606233

``` r
cat("Mean of z-scores:", mean_z, "\n")
```

    ## Mean of z-scores: 1.387779e-16

``` r
cat("Standard Deviation of z-scores:", sd_z, "\n")
```

    ## Standard Deviation of z-scores: 1

``` r
masses <- c(20.48, 35.97, 62.34)
sdmass <- c(0.21, 0.46, 0.54)

mean_masses <- sum(masses)
sd_masses <- sqrt(sum(sdmass^2))

cat("Mean of the sum of the masses: ", mean_masses, "g\n")
```

    ## Mean of the sum of the masses:  118.79 g

``` r
cat("Standart Deviation of the sum of the masses: ", sd_masses,"g\n")
```

    ## Standart Deviation of the sum of the masses:  0.7397973 g

``` r
x <- c(6,9,12,15,18)
p <- c(0.1, 0.2, 0.4, 0.2, 0.1)

mu <- sum(x*p)
variance <- sum(p*(x-mu)^2)

cat("Mean", mu)
```

    ## Mean 12

``` r
cat("Variance", variance)
```

    ## Variance 10.8

``` r
samplesize <- 2
samples <- expand.grid(x,x)
samples$Mean <- rowMeans(samples)

meanprob <- table(samples$Mean)/(length(x)^samplesize)

meanprobdf <- as.data.frame(meanprob)
colnames(meanprobdf) <- c("Sample Mean", "Probability")

print("Possible Sample Means and their Probability:")
```

    ## [1] "Possible Sample Means and their Probability:"

``` r
print(meanprobdf)
```

    ##   Sample Mean Probability
    ## 1           6        0.04
    ## 2         7.5        0.08
    ## 3           9        0.12
    ## 4        10.5        0.16
    ## 5          12        0.20
    ## 6        13.5        0.16
    ## 7          15        0.12
    ## 8        16.5        0.08
    ## 9          18        0.04
