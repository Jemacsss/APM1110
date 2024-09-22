FA4_Macagba
================
2024-09-22

``` r
normal <- c(67, 70, 63, 65, 68, 60, 70, 64, 69, 61, 66, 65, 71, 62, 66, 68, 64, 67, 62, 66, 65, 63, 66, 65, 63, 69, 62, 67, 59, 66, 65, 63, 65, 60, 67, 64, 68, 61, 69, 65, 62, 67, 70, 64, 63, 68, 64, 61, 66, 66)

skewed_right <- c(31, 43, 30, 30, 38, 26, 29, 55, 46, 26, 29, 57, 34, 34, 36, 40, 28, 26, 66, 63, 30, 33, 35, 34, 29, 40, 24, 29, 24, 27, 35, 33, 75, 38, 34, 85, 29, 40, 41, 35, 26, 34, 19, 23, 28, 26, 31, 25, 22, 28)

skewed_left <- c(102, 55, 70, 95, 73, 79, 60, 73, 79, 60, 73, 89, 85, 72, 92, 76, 93, 76, 97, 10, 70, 85, 25, 83, 58, 10, 92, 82, 87, 104, 75, 80, 66, 93, 90, 84, 73, 98, 79, 35, 71, 90, 71, 63, 58, 82, 72, 93, 44, 65, 77, 81, 77)

uniform <- c(12.1, 12.1, 12.4, 12.1, 12.1, 12.2, 12.2, 12.2, 11.9, 12.2, 12.3, 12.3, 11.7, 12.3, 12.3, 12.4, 12.4, 12.1, 12.4, 12.4, 12.5, 11.8, 12.5, 12.5, 12.5, 11.6, 11.6, 12.0, 11.6, 11.6, 11.7, 12.3, 11.7, 11.7, 11.7, 11.8, 12.5, 11.8, 11.8, 11.8, 11.9, 11.9, 11.9, 12.2, 11.9, 12.0, 12.0, 12.0)
```

``` r
library(moments)


first_moment <- function(x) mean(x)
second_moment <- function(x) mean(x^2)
third_moment <- function(x) mean(x^3)
fourth_moment <- function(x) mean(x^4)


moments_data <- function(x) {
  return(list(
    first_moment = first_moment(x),
    second_moment = second_moment(x),
    third_moment = third_moment(x),
    fourth_moment = fourth_moment(x)
  ))
}


moments_normal <- moments_data(normal)
moments_skewed_right <- moments_data(skewed_right)
moments_skewed_left <- moments_data(skewed_left)
moments_uniform <- moments_data(uniform)

cat("Normal Data Moments:\n")
```

    ## Normal Data Moments:

``` r
print(moments_normal)
```

    ## $first_moment
    ## [1] 65.14
    ## 
    ## $second_moment
    ## [1] 4251.54
    ## 
    ## $third_moment
    ## [1] 278028.3
    ## 
    ## $fourth_moment
    ## [1] 18216656

``` r
cat("\nSkewed-Right Data Moments:\n")
```

    ## 
    ## Skewed-Right Data Moments:

``` r
print(moments_skewed_right)
```

    ## $first_moment
    ## [1] 35.58
    ## 
    ## $second_moment
    ## [1] 1443.02
    ## 
    ## $third_moment
    ## [1] 68503.74
    ## 
    ## $fourth_moment
    ## [1] 3805104

``` r
cat("\nSkewed-Left Data Moments:\n")
```

    ## 
    ## Skewed-Left Data Moments:

``` r
print(moments_skewed_left)
```

    ## $first_moment
    ## [1] 74
    ## 
    ## $second_moment
    ## [1] 5876.226
    ## 
    ## $third_moment
    ## [1] 482471.6
    ## 
    ## $fourth_moment
    ## [1] 40568234

``` r
cat("\nUniform Data Moments:\n")
```

    ## 
    ## Uniform Data Moments:

``` r
print(moments_uniform)
```

    ## $first_moment
    ## [1] 12.06042
    ## 
    ## $second_moment
    ## [1] 145.5352
    ## 
    ## $third_moment
    ## [1] 1757.182
    ## 
    ## $fourth_moment
    ## [1] 21227.92

``` r
first_central_moment <- function(x) 0

second_central_moment <- function(x) mean((x - mean(x))^2)

third_central_moment <- function(x) mean((x - mean(x))^3)

fourth_central_moment <- function(x) mean((x - mean(x))^4)

calculate_central_moments <- function(x) {
  return(list(
    first_central_moment = first_central_moment(x),
    second_central_moment = second_central_moment(x),
    third_central_moment = third_central_moment(x),
    fourth_central_moment = fourth_central_moment(x)
  ))
}

central_moments_normal <- calculate_central_moments(normal)
central_moments_skewed_right <- calculate_central_moments(skewed_right)
central_moments_skewed_left <- calculate_central_moments(skewed_left)
central_moments_uniform <- calculate_central_moments(uniform)


cat("Central Moments for Normal Data:\n")
```

    ## Central Moments for Normal Data:

``` r
print(central_moments_normal)
```

    ## $first_central_moment
    ## [1] 0
    ## 
    ## $second_central_moment
    ## [1] 8.3204
    ## 
    ## $third_central_moment
    ## [1] -0.957312
    ## 
    ## $fourth_central_moment
    ## [1] 161.0172

``` r
cat("\nCentral Moments for Skewed-Right Data:\n")
```

    ## 
    ## Central Moments for Skewed-Right Data:

``` r
print(central_moments_skewed_right)
```

    ## $first_central_moment
    ## [1] 0
    ## 
    ## $second_central_moment
    ## [1] 177.0836
    ## 
    ## $third_central_moment
    ## [1] 4559.819
    ## 
    ## $fourth_central_moment
    ## [1] 208496.2

``` r
cat("\nCentral Moments for Skewed-Left Data:\n")
```

    ## 
    ## Central Moments for Skewed-Left Data:

``` r
print(central_moments_skewed_left)
```

    ## $first_central_moment
    ## [1] 0
    ## 
    ## $second_central_moment
    ## [1] 400.2264
    ## 
    ## $third_central_moment
    ## [1] -11602.64
    ## 
    ## $fourth_central_moment
    ## [1] 866200.7

``` r
cat("\nCentral Moments for Uniform Data:\n")
```

    ## 
    ## Central Moments for Uniform Data:

``` r
print(central_moments_uniform)
```

    ## $first_central_moment
    ## [1] 0
    ## 
    ## $second_central_moment
    ## [1] 0.08155816
    ## 
    ## $third_central_moment
    ## [1] -0.0006487811
    ## 
    ## $fourth_central_moment
    ## [1] 0.01170934

``` r
first_moment_about_75 <- function(x) mean(x - 75)


second_moment_about_75 <- function(x) mean((x - 75)^2)


third_moment_about_75 <- function(x) mean((x - 75)^3)


fourth_moment_about_75 <- function(x) mean((x - 75)^4)


moments_about_75 <- function(x) {
  return(list(
    first_moment_about_75 = first_moment_about_75(x),
    second_moment_about_75 = second_moment_about_75(x),
    third_moment_about_75 = third_moment_about_75(x),
    fourth_moment_about_75 = fourth_moment_about_75(x)
  ))
}


moments_female_heights_about_75 <- moments_about_75(normal)


cat("Moments about 75 for Female Height Data:\n")
```

    ## Moments about 75 for Female Height Data:

``` r
print(moments_female_heights_about_75)
```

    ## $first_moment_about_75
    ## [1] -9.86
    ## 
    ## $second_moment_about_75
    ## [1] 105.54
    ## 
    ## $third_moment_about_75
    ## [1] -1205.66
    ## 
    ## $fourth_moment_about_75
    ## [1] 14503.86

``` r
moments_about_mean <- function(x) {
  return(list(
    first_moment_about_mean = 0,  

    second_moment_about_mean = second_central_moment(x),
    third_moment_about_mean = third_central_moment(x),
    fourth_moment_about_mean = fourth_central_moment(x)
  ))
}


moments_about_mean_normal <- moments_about_mean(normal)


moments_about_75_normal <- moments_about_75(normal)


m1_prime <- moments_about_75_normal$first_moment_about_75
m2_prime <- moments_about_75_normal$second_moment_about_75
m3_prime <- moments_about_75_normal$third_moment_about_75
m4_prime <- moments_about_75_normal$fourth_moment_about_75

m2_central <- moments_about_mean_normal$second_moment_about_mean
m3_central <- moments_about_mean_normal$third_moment_about_mean
m4_central <- moments_about_mean_normal$fourth_moment_about_mean


m2_formula <- m2_prime - (m1_prime)^2
cat("Second moment formula check (m2):", m2_formula == m2_central, "\n")
```

    ## Second moment formula check (m2): FALSE

``` r
m3_formula <- m3_prime - 3*m1_prime*m2_prime + 2*(m1_prime)^3
cat("Third moment formula check (m3):", m3_formula == m3_central, "\n")
```

    ## Third moment formula check (m3): FALSE

``` r
m4_formula <- m4_prime - 4*m1_prime*m3_prime + 6*m1_prime*m2_prime - 3*(m1_prime)^4
cat("Fourth moment formula check (m4):", m4_formula == m4_central, "\n")
```

    ## Fourth moment formula check (m4): FALSE
