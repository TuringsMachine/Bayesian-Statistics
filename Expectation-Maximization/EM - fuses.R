dat = read.csv("Acme Corporation - fuses.csv")

X = dat$X1

hist(X)

## Initialize the parameters

w     = 1/2                         

lambda = 1/rnorm(1, mean(X), sd(X))

mu =  log(rnorm(1, mean(X), sd(X)))

tau = log(sd(X))



w      = 0.1

mu     = mean(log(X))

tau    = sd(log(X))

lambda = 20/mean(X)



s = 0

sw = FALSE

QQ = -Inf

QQ.out = NULL

epsilon = 10^(-5)


while(!sw)

{

  #calculating V for expectation

  V = array(0,dim=c(length(X),2))

  for(i in 1:length(X))

  {

    V[i,1] = w*dexp(X[i], lambda)

    V[i,2] = (1-w)*dlnorm(X[i], mu, tau) 

    V[i, ] = V[i, ]/sum(V[i, ])

  }


  #calculations for maximizations

  mu = 0

  lambda = 0

  tau = 0

  for(j in 1:length(X)){

    lambda = lambda + V[j,1]*X[j]

    mu = mu + V[j,2]*log(X[j])

    

  }

  lambda = sum(V[,1])/lambda

  mu = mu/sum(V[,2])

  w = mean(V[,1])




  

  for(j in 1:length(X)){

    tau = tau + V[j,2]*(log(X[j]) - mu)^2

  }

  tau = sqrt(tau/sum(V[,2]))

  

  #Convergence

  QQn = 0

  for(i in 1:length(X))

  {

    QQn = QQn + V[i,1]*(log(w)+ log(dexp(X[i], lambda))) 

              + V[i,2]*(log(1-w) + dlnorm(X[i], mu, tau, log = TRUE))

  }


  if((abs(QQn-QQ)/abs(QQn))<epsilon){

    sw=TRUE

  }

  QQ = QQn

}

