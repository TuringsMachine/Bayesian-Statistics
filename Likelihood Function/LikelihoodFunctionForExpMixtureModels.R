n     = 100           # Size of the sample to be generated
w     = c(0.3, 0.25, 0.25, 0.2)  # Weights
mu    = c(1, 4, 7, 10)      # Means
cc    = sample(1:4, n, replace=T, prob=w)
x     = rexp(n, 1/ mu[cc])
#calculating overall mean and variance
mean =  mean(x)    #5.05
var = var(x)