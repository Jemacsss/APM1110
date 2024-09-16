MACAGBA_FA2
================
2024-09-17

``` r
N <- 5
X_j <- c(1,2,3,4,5)

lside <- sum((X_j-1)^2)

rside <- sum(X_j^2) - 2 * sum(X_j) + N

cat(lside==rside)
```

    ## TRUE

``` r
cat("Left Side:",lside, "\nRight Side:",rside)
```

    ## Left Side: 30 
    ## Right Side: 30

``` r
U <- c(3,-2,5)
V <- c(-4,-1,6)

a <- sum(U*V)
cat("1. a)", a, "\n")
```

    ## 1. a) 20

``` r
b <- sum((U+3)*(V-4))
cat("2. b)", b, "\n")
```

    ## 2. b) -37

``` r
c <- sum(V^2)
cat("3. c)", c, "\n")
```

    ## 3. c) 53

``` r
d <- sum(U) * sum(V)^2
cat("4. d)", d, "\n")
```

    ## 4. d) 6

``` r
e <- sum(U*V^2)
cat("5. e)", e, "\n")
```

    ## 5. e) 226

``` r
f <- sum(U^2 - 2*V^2 + 2)
cat("6. f)", f, "\n")
```

    ## 6. f) -62

``` r
g <- sum(U/V)
cat("7. g)", g, "\n")
```

    ## 7. g) 2.083333

``` r
set_a <- c(3,5,8,3,7,2) 
geometricmean_a <- prod(set_a)^(1/length(set_a))
cat("geometric mean of set a: ",geometricmean_a, "\n")
```

    ## geometric mean of set a:  4.140681

``` r
set_b <- c(28.5, 73.6, 47.2, 31.5, 64.8) 
geometricmean_b <- prod(set_b)^(1/length(set_b))
cat("geometric mean of set b: ",geometricmean_b, "\n")
```

    ## geometric mean of set b:  45.8258
