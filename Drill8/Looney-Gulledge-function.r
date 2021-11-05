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


#=============================================================
# Testing speeds
#-------------------------------------------------------------
# small n
#-------------------------------------------------------------
# iter = 1E3 
system.time( EPP1(n=3, iter=1E3) )
system.time( EPP2(n=3, iter=1E3) )  # a little bit better 

# iter = 1E5
system.time( EPP1(n=3, iter=1E5) )   
system.time( EPP2(n=3, iter=1E5) )  # better 

# iter = 1E6 (try at home)
system.time( EPP1(n=3, iter=1E6) )   
system.time( EPP2(n=3, iter=1E6) )  # much better 


#-------------------------------------------------------------
# very large n
#-------------------------------------------------------------
# iter = 1E3
system.time( EPP1(n=1000, iter=1E3) )
system.time( EPP2(n=1000, iter=1E3) )   # Worse 

# iter = 1E4 
system.time( EPP1(n=1000, iter=1E4) )
system.time( EPP2(n=1000, iter=1E4) )   # Worse 

# iter = 1E5 
system.time( EPP1(n=1000, iter=1E5) )   
system.time( EPP2(n=1000, iter=1E5) )  # a little bit better 

# iter = 1E6  (check at home due to long execution time)
system.time( EPP1(n=1000, iter=1E6) )
system.time( EPP2(n=1000, iter=1E6) )  # Memory problem


#-------------------------------------------------------------
# Conclusion
#-------------------------------------------------------------
#
# EPP1 is OK when iter is small. 
# EPP2 outperforms when iter >>> n. (In practice, we need this).
# 
# Why does it happen?
#   




