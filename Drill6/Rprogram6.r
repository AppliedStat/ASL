

#=================================================================
# 1 
# NOTE: K1 = average values of sum(Yi-b0-b1*Xi)^2 / sigma^2
ITER = 100000
sigma = 5
n = 10

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      b0 = runif(1)                   # any number
      b1 = rlogis(1)                  # any number 
      e = rnorm(n, mean=0, sd=sigma)  # N(0,sigma^2) <--- normal only
      Xi = rexp(n)                    # Any n observations
      Yi = b0 + b1*Xi + e 
      SUM[i] = sum((Yi-b0-b1*Xi)^2) 
  }
  mean(SUM) / sigma^2


#=================================================================
# 2 
# NOTE: K2 = average values of sum(Yi-b0hat-b1hat*Xi)^2 / sigma^2
ITER = 100000
sigma = 10
n = 12
  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      b0 = runif(1)                   # any number
      b1 = rlogis(1)                  # any number 
      e = rnorm(n, mean=0, sd=sigma)  # N(0,sigma^2) <--- normal only
      Xi = rexp(n)                    # Any n observations
      Yi = b0 + b1*Xi + e 

      Xbar = mean(Xi)
      Ybar = mean(Yi)
      b1hat = ( sum(Xi*Yi) - n*Xbar*Ybar ) / (sum(Xi^2)-n*Xbar^2)
      b0hat = Ybar - b1hat*Xbar 
      SUM[i] = sum((Yi-b0hat-b1hat*Xi)^2) 
  }
  mean(SUM) / sigma^2


