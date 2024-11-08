FA8_Macagba
================
2024-11-08

``` r
plant_data <- read.csv("C:/Users/Jec/Documents/FEU/Stat Theory/plantgrowth.csv")


head(plant_data)
```

    ##   weight group
    ## 1   4.17  ctrl
    ## 2   5.58  ctrl
    ## 3   5.18  ctrl
    ## 4   6.11  ctrl
    ## 5   4.50  ctrl
    ## 6   4.61  ctrl

``` r
str(plant_data)
```

    ## 'data.frame':    30 obs. of  2 variables:
    ##  $ weight: num  4.17 5.58 5.18 6.11 4.5 4.61 5.17 4.53 5.33 5.14 ...
    ##  $ group : chr  "ctrl" "ctrl" "ctrl" "ctrl" ...

``` r
summary(plant_data)
```

    ##      weight         group          
    ##  Min.   :3.590   Length:30         
    ##  1st Qu.:4.550   Class :character  
    ##  Median :5.155   Mode  :character  
    ##  Mean   :5.073                     
    ##  3rd Qu.:5.530                     
    ##  Max.   :6.310

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
descriptive_stats <- plant_data %>%
  group_by(group) %>%
  summarise(
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight = sd(weight, na.rm = TRUE),
    n = n()
  )
print(descriptive_stats)
```

    ## # A tibble: 3 × 4
    ##   group mean_weight sd_weight     n
    ##   <chr>       <dbl>     <dbl> <int>
    ## 1 ctrl         5.03     0.583    10
    ## 2 trt1         4.66     0.794    10
    ## 3 trt2         5.53     0.443    10

``` r
shapiro_test_results <- plant_data %>%
  group_by(group) %>%
  summarise(
    shapiro_p_value = shapiro.test(weight)$p.value
  )
print(shapiro_test_results)
```

    ## # A tibble: 3 × 2
    ##   group shapiro_p_value
    ##   <chr>           <dbl>
    ## 1 ctrl            0.747
    ## 2 trt1            0.452
    ## 3 trt2            0.564

``` r
shapiro_test_results <- plant_data %>%
  group_by(group) %>%
  summarise(
    shapiro_p_value = shapiro.test(weight)$p.value
  )
print(shapiro_test_results)
```

    ## # A tibble: 3 × 2
    ##   group shapiro_p_value
    ##   <chr>           <dbl>
    ## 1 ctrl            0.747
    ## 2 trt1            0.452
    ## 3 trt2            0.564

``` r
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

``` r
leveneTest(weight ~ group, data = plant_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  2  1.1192 0.3412
    ##       27

``` r
anova_result <- aov(weight ~ group, data = plant_data)


summary(anova_result)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## group        2  3.766  1.8832   4.846 0.0159 *
    ## Residuals   27 10.492  0.3886                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
tukey_result <- TukeyHSD(anova_result)


print(tukey_result)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = weight ~ group, data = plant_data)
    ## 
    ## $group
    ##             diff        lwr       upr     p adj
    ## trt1-ctrl -0.371 -1.0622161 0.3202161 0.3908711
    ## trt2-ctrl  0.494 -0.1972161 1.1852161 0.1979960
    ## trt2-trt1  0.865  0.1737839 1.5562161 0.0120064

``` r
library(glue)


summary_result <- summary(anova_result)


df_between <- summary_result[[1]]$Df[1]
df_within <- summary_result[[1]]$Df[2]
F_value <- summary_result[[1]]$`F value`[1]
p_value <- summary_result[[1]]$`Pr(>F)`[1]

tukey_result <- TukeyHSD(anova_result)


tukey_significant <- sapply(tukey_result$group[, "p adj"], function(p) ifelse(p < 0.05, "significant", "not significant"))

means <- descriptive_stats$mean_weight
sds <- descriptive_stats$sd_weight
groups <- descriptive_stats$group


report <- glue("
A one-way ANOVA was conducted to examine the effect of group on plant weight. The results indicated that there was a statistically significant difference in plant weights between groups, F({df_between}, {df_within}) = {round(F_value, 2)}, p = {round(p_value, 3)}. 
Post-hoc comparisons using the Tukey HSD test revealed that:
  - Group 1 vs Group 2 was {tukey_significant[1]}.
  - Group 1 vs Group 3 was {tukey_significant[2]}.
  - Group 2 vs Group 3 was {tukey_significant[3]}.

Descriptive statistics showed that the mean plant weights for the groups were as follows: 
Group {groups[1]} (M = {round(means[1], 2)}, SD = {round(sds[1], 2)}), 
Group {groups[2]} (M = {round(means[2], 2)}, SD = {round(sds[2], 2)}), 
and Group {groups[3]} (M = {round(means[3], 2)}, SD = {round(sds[3], 2)}).
")

cat(report)
```

    ## A one-way ANOVA was conducted to examine the effect of group on plant weight. The results indicated that there was a statistically significant difference in plant weights between groups, F(2, 27) = 4.85, p = 0.016. 
    ## Post-hoc comparisons using the Tukey HSD test revealed that:
    ##   - Group 1 vs Group 2 was not significant.
    ##   - Group 1 vs Group 3 was not significant.
    ##   - Group 2 vs Group 3 was significant.
    ## 
    ## Descriptive statistics showed that the mean plant weights for the groups were as follows: 
    ## Group ctrl (M = 5.03, SD = 0.58), 
    ## Group trt1 (M = 4.66, SD = 0.79), 
    ## and Group trt2 (M = 5.53, SD = 0.44).
