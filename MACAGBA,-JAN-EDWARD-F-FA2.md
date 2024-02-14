FA2_Macgba
================
2024-02-14

2)  An experiment consists of tossing two fair coins. Use R to simulate
    this experiment 100 times and obtain the relative frequency of each
    possible outcome. Hence, estimate the probability of getting one
    head and one tail in any order.

``` r
coin_toss = 100

coin <- c('heads', 'tails')
flip <- sample(coin, size = coin_toss, replace = TRUE)

freq_heads <- cumsum(flip == 'heads') / (1:coin_toss)
freq_tails <- cumsum(flip == 'tails') / (1:coin_toss)

plot(1:coin_toss, freq_heads, type = "l", lwd = 2, col = 'skyblue', 
     ylim = c(0, 1),
     xlab = 'Number of Tosses', ylab = 'Relative Frequency', las = 1)

lines(1:coin_toss, freq_tails, type = "l", lwd = 2, col = 'pink')
abline(h = 0.5, col = 'black')

legend("topright", legend = c("Heads", "Tails"), col = c('skyblue', 'pink'), 
       lwd = 2)
```

![](MACAGBA,-JAN-EDWARD-F-FA2_files/figure-gfm/coin%20toss-1.png)<!-- -->

3)  An experiment consists of rolling a die. Use R to simulate this
    experiment 600 times and obtain the relative frequency of each
    possible outcome. Hence, estimate the probability of getting each of
    1, 2, 3, 4, 5, and 6.

``` r
dice_roll = 600
dice <- sample(1:6, size = dice_roll, replace = TRUE)
relative_freq <- table(dice) / dice_roll
colors <- c('#92A8D1', '#034078', '#9CC3D5', '#D9BF77', '#ACD8AA', '#6B4226')
barplot(relative_freq, names.arg = 1:6,
        col = colors, main = 'Possible Outcoumes',
        xlab = 'Outcomes', ylab = 'Relative Frequency')
```

![](MACAGBA,-JAN-EDWARD-F-FA2_files/figure-gfm/dice%20roll-1.png)<!-- -->
