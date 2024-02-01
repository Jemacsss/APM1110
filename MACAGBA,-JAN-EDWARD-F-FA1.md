MACAGBA, JAN EDWARD F.-FA1
================
2024-02-01

``` r
library(e1071)
library(tinytex)

results=read.csv("C:/Users/asus/Documents/Macagba_R/data.csv",header=T)
results
```

    ##     gender arch1 prog1 arch2 prog2
    ## 1        m    99    98    83    94
    ## 2        m    NA    NA    86    77
    ## 3        m    97    97    92    93
    ## 4        m    99    97    95    96
    ## 5        m    89    92    86    94
    ## 6        m    91    97    91    97
    ## 7        m   100    88    96    85
    ## 8        f    86    82    89    87
    ## 9        m    89    88    65    84
    ## 10       m    85    90    83    85
    ## 11       m    50    91    84    93
    ## 12       m    96    71    56    83
    ## 13       f    98    80    81    94
    ## 14       m    96    76    59    84
    ## 15       m    73    72    91    87
    ## 16       m    67    82    80    77
    ## 17       m    80    85    94    72
    ## 18       m    91    76    85    84
    ## 19       m    89    81    77    81
    ## 20       m    77    81    88    91
    ## 21       m    71    82    59    79
    ## 22       m    84    81    88    77
    ## 23       m    95    83    92    63
    ## 24       m     3    87    56    76
    ## 25       f    95    65    63    82
    ## 26       f    NA    NA    91    65
    ## 27       m    59    79    73    82
    ## 28       m    95    83    49    69
    ## 29       m    80    80    87    72
    ## 30       m    97    92    98    96
    ## 31       m    81    89    41    57
    ## 32       m    77    70    51    71
    ## 33       m    69    74    83    68
    ## 34       m    82    79    57    45
    ## 35       f    85    66    56    67
    ## 36       m    87    68    56    78
    ## 37       m    88    76    47    61
    ## 38       m    83    76    41    65
    ## 39       m    51    67    49    79
    ## 40       f    76    63    57    76
    ## 41       m    88    64    48    53
    ## 42       m    61    53    54    61
    ## 43       m    83    60    56    49
    ## 44       m    90    78    81    50
    ## 45       m    40    67    53    68
    ## 46       m    92    61    47    64
    ## 47       m    76    69    44    59
    ## 48       m    72    61    62    56
    ## 49       f    77    53    48    60
    ## 50       m    58    52    50    73
    ## 51       m    63    62    40    48
    ## 52       m    48    73    74    53
    ## 53       m    40    75    43    52
    ## 54       m    40    40    48    62
    ## 55       m    75    67    40    45
    ## 56       f    49    61    49    44
    ## 57       m    54    47    43    52
    ## 58       m    56    55    44    55
    ## 59       m    75    40    40    51
    ## 60       m    64    86    50    81
    ## 61       f    88    40    43    83
    ## 62       m    82    66    51    63
    ## 63       m    73    64    28    54
    ## 64       f    59    28    60    51
    ## 65       m    74    57    45    61
    ## 66       m    45    69    35    40
    ## 67       m    70    52    40    43
    ## 68       m    74    29    44    52
    ## 69       m    43    25    31    14
    ## 70       m    49    69    40    24
    ## 71       m    45    29    32    25
    ## 72       m    74    71    40    46
    ## 73       m    46    56    50    28
    ## 74       m    56    52    42    57
    ## 75       m    16    33    16     9
    ## 76       m    21    25    26    12
    ## 77       m    47    56    43    16
    ## 78       m    77    60    47    62
    ## 79       m    27    40    37     6
    ## 80       m    74    13    40    18
    ## 81       f    16    14    NA    NA
    ## 82       m    14    31    14    20
    ## 83       m    23    54    48    NA
    ## 84       m    83    76    58    75
    ## 85       f    NA    15    16    NA
    ## 86       m    45    40    40    61
    ## 87       m    40    28    26     9
    ## 88       m    48    27    23    16
    ## 89       m    91    89     6    73
    ## 90       f    50    27    22    11
    ## 91       m    77    82    45    65
    ## 92       m    49    49    36    31
    ## 93       m    96    84    48    29
    ## 94       f    21    29    25     5
    ## 95       m    61    40    34    11
    ## 96       m    50    19    41    NA
    ## 97       f    68    74    30    48
    ## 98       m    50    40    51    56
    ## 99       m    69    59    25    40
    ## 100      m    60    36    40    28
    ## 101      f    43    14    NA    NA
    ## 102      m    43    30    40    14
    ## 103      m    47    68    43    34
    ## 104      f    60    47    40    NA
    ## 105      m    40    68    57    75
    ## 106      m    45    26    38     6
    ## 107      m    45    31    NA    NA
    ## 108      f    31    21    32     8
    ## 109      m    49    12    24    14
    ## 110      m    87    40    40    32
    ## 111      m    40    76    49    17
    ## 112      f     8    29    15    14
    ## 113      m    62    46    50    31
    ## 114      m    14    21    NA    NA
    ## 115      m     7    25    27     7
    ## 116      m    16    27    25     7
    ## 117      m    73    51    48    23
    ## 118      m    56    54    49    25
    ## 119      m    46    64    13    19

``` r
skewness(results$arch1, na.rm = T)
```

    ## [1] -0.5063276

``` r
skewness(results$arch2, na.rm = T)
```

    ## [1] 0.4423272

``` r
skewness(results$prog1, na.rm = T)
```

    ## [1] -0.329161

``` r
skewness(results$prog2, na.rm = T)
```

    ## [1] -0.2977574

In the data it would be seen that all of the examination subjects are
all negative except for arch2 which is positive. It can be also seen
that the prog1 and prog2 are closer to each other while arch1 is the one
skewed to the very left.

------------------------------------------------------------------------

``` r
skew=function(x){
  mean=mean(x, na.rm = T)
  median=median(x, na.rm = T)
  sd=sd(x, na.rm = T)
  
  skewness=3*(mean-median)/sd
  
  return(skewness)
}

arch1_skewness=skew(results$arch1)
arch2_skewness=skew(results$arch2)
prog1_skewness=skew(results$prog1)
prog2_skewness=skew(results$prog2)

print(arch1_skewness)
```

    ## [1] -0.6069042

``` r
print(arch2_skewness)
```

    ## [1] 0.5421286

``` r
print(prog1_skewness)
```

    ## [1] -0.643229

``` r
print(prog2_skewness)
```

    ## [1] -0.3562908

When using Pearsons equation, the data increased especially prog1 from a
-0.329 it became a -0.643 the other data increased by 1. I would say
that using Pearsons equation has risk to it because there would be
inaccuracies in the data.

------------------------------------------------------------------------

``` r
Females=c(57,59,78,79,60,65,68,71,75,48,51,55,56,41,43,44,75,78,80,81,83,83,85)
Males=c(48,49,49,30,30,31,32,35,37,41,86,42,51,53,56,42,44,50,51,65,67,51,56,
        58,64,64,75)

stem(Females)
```

    ## 
    ##   The decimal point is 1 digit(s) to the right of the |
    ## 
    ##   4 | 1348
    ##   5 | 15679
    ##   6 | 058
    ##   7 | 155889
    ##   8 | 01335

``` r
stem(Males)
```

    ## 
    ##   The decimal point is 1 digit(s) to the right of the |
    ## 
    ##   3 | 001257
    ##   4 | 1224899
    ##   5 | 01113668
    ##   6 | 4457
    ##   7 | 5
    ##   8 | 6

The advantages of using the stem and leaf display is that for small data
it would be much easier to find out the precise data instead of using a
histogram. A stem and leaf would be easier to create than a histogram
and also you would be seeing the individual data and not just different
bars.

------------------------------------------------------------------------

``` r
Females=c(57,59,78,79,60,65,68,71,75,48,51,55,56,41,43,44,75,78,80,81,83,83,85)
Males=c(48,49,49,30,30,31,32,35,37,41,86,42,51,53,56,42,44,50,51,65,67,51,56,
        58,64,64,75)

boxplot(Females, Males, names=c("Females", "Males"), main="Grades by Gender", 
        ylab="Grades",col=c("#FFE5E5","#756AB6"))
```

![](MACAGBA,-JAN-EDWARD-F-FA1_files/figure-gfm/boxplot-1.png)<!-- -->
The finding for the boxplot is that the grades for the Female group is
higher than the Male group with the median of the Female group being
higher.
