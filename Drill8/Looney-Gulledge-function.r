# 
#-------------------------------------------------------------
# Empirical Percentage Points for Correlation Test.
# See Table 2. (Looney and Gulledge, 1985)
#-------------------------------------------------------------
EPP1 <- function(n, levels=c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995), iter=10000) { 
    levels = sort(levels)
    COR = NULL
    for ( i in seq_len(iter) ) {
        z = sort(rnorm(n))
        q = qnorm(ppoints(n))
        COR = c(COR, cor(z,q))
    }

    if ( levels[1] <= 0 ) {
         min.cor = min(COR)
         names(min.cor) = "0%"
         round(c(min.cor, quantile(COR, prob=levels[-1], type=5)),4)
    } else { 
         round(c(quantile(COR, prob=levels, type=5)),4) 
    }
}
#
set.seed(1); EPP1(n=3)
set.seed(1); EPP1(n=3, levels=c(0.005, 0.010) )
set.seed(1); EPP1(n=3, levels=c(0, 0.005, 0.010) )
set.seed(1); EPP1(n=3, levels=c(0.05, 0.01, 0) )
#-------------------------------------------------------------


#-------------------------------------------------------------
# HW: Can you improve the above R program by avoiding for() loop.
EPP2 <- function(n, levels=c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995), iter=10000) {
   levels=sort(levels)
   mycor = function(z) {cor(z,qnorm(ppoints(n)))}  # lexical scoping for n
   zz    = apply(matrix(rnorm(n*iter), ncol=n), 1, sort)
   COR   = apply(zz, 2, mycor)

    if ( levels[1] <= 0 ) {
         min.cor = min(COR)
         names(min.cor) = "0%"
         round(c(min.cor, quantile(COR, prob=levels[-1], type=5)),4)
    } else { 
         round(c(quantile(COR, prob=levels, type=5)),4) 
    }
}
#

set.seed(1); EPP2(n=3)
set.seed(1); EPP2(n=3, levels=c(0.005, 0.010) )
set.seed(1); EPP2(n=3, levels=c(0, 0.005, 0.010) )
set.seed(1); EPP2(n=3, levels=c(0.05, 0.01, 0) )
#


#-------------------------------------------------------------
# iter = 10000
system.time( EPP1(n=3))
system.time( EPP2(n=3))


# iter = 1E5
system.time( EPP1(n=3, iter=1E5))
system.time( EPP2(n=3, iter=1E5))

