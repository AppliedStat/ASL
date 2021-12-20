
#####################################
# Binomial and Beta
# Type your own value for n, p, and x.
n = 1L + round(100*runif(1))
p = runif(1L)
x = min(50*runif(1), n) # 0 < x < n.
c(x, n, p)
# Compare the following two:
c(pbinom(x, size=n, prob=p), pbeta(1-p, n-floor(x), floor(x)+1) )


#====================================
# Type your own value for n, p, and k.
# Assume that X is from Bin(n,p)
n = 4L
p = 1/2
k = 2L
# p = runif(1L)
# k   = sample(1L:(n-1L), size=1L)
 
# True value of P[X <= k]
pbinom(k, size=n, prob=p)
 
# Simulated value of  P[X <= k]
# set.seed(1L)
ITER = 100000L
countA = countB = 0L
 
for ( i in seq_len(ITER) ) {
     X = runif(n)
     if ( sum(X <= p) <= k ) countA = countA + 1L
 
     Y = sort(X)
     if( Y[k+1L] > p ) countB= countB + 1L
}
countA / ITER
countB / ITER


#####################################
# Binomial and Negative binomial
n = 2L
r = 2L
# p = 1/4
p = runif(1L)

c(pbinom(r-1, n,p), 1-pnbinom(n-r,r,p) )


# NOTE: wiki (Cumulative distribution function)
# https://en.wikipedia.org/wiki/Negative_binomial_distribution
# It works only with p=1/2.
# p=1/2 
k = 2
c(pnbinom(k, r, p), pbinom(k, k+r,p) )



#####################################
# BETA 
x = runif(1L)
a = 10*runif(1)
b = 10*runif(1)

c(dbeta(x, a,b), dbeta(1-x, b,a) )

c(pbeta(x, a,b), 1-pbeta(1-x, b,a) )

#####################################


