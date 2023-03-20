n     = 200           # Size of the sample to be generated
w     = c(0.7, 0.2, 0.1)  # Weights
mu    = c(1, 2, 6)      # Means
cc    = sample(1:3, n, replace=T, prob=w)
x     = rpois(n, mu[cc])
empfreq = table(factor(x, levels=seq(0, max(x))))/n  
barplot(empfreq)  