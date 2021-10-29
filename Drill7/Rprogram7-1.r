
#===================================================
pdf(file="F-test.pdf", width=5.5, height=3.0)
par(mfrow=c(1,1), mar=c(4, 4, 1, 1), omi=c(0,0,0,0), cex=0.8, mex=0.8)
#---------------------------------------------------


ITER = 500
sigma = 5
n = 10
alpha = 0.05
Xi = 1:n

BETA1 = seq(-2,2, by=0.05)


#===================================================
# Simulated power
#---------------------------------------------------
sim.power = numeric(length(BETA1))
for ( j in seq_along(BETA1) ) { 
    beta0 = runif(1)                      # any number
    for ( iter in seq_len(ITER) ) {
        ei = rnorm(n, mean=0, sd=sigma)   # N(0,sigma^2) <--- normal only
        Yi = beta0 + BETA1[j]*Xi + ei

        Xbar = mean(Xi)
        Ybar = mean(Yi)
        Sxy = sum(Xi*Yi) - n*Xbar*Ybar
        Sxx = sum(Xi^2)-n*Xbar^2

        b1hat = Sxy / Sxx
        b0hat = Ybar - b1hat*Xbar 

        Yihat = b0hat + b1hat*Xi
        MSR = sum( (Yihat-Ybar)^2 ) / (2-1)
        MSE = sum( (Yi - Yihat)^2 ) / (n-2)
        Fstat = MSR / MSE 
        F.cut = qf(1-alpha, df1=(2-1), df2=(n-2) )
        if ( Fstat > F.cut )  sim.power[j] = sim.power[j] + 1/ITER
    }
}


#===================================================
# Theoretical power
#---------------------------------------------------
Kf = function(beta1, alpha, Xi,sigma) { 
     n = length(Xi)
     Xbar = mean(Xi)
     Sxx =  (sum(Xi^2)-n*Xbar^2)
     delta = beta1^2*Sxx / sigma^2
     F.cut = qf(1-alpha, df1=(2-1), df2=(n-2) )
     1-pf(F.cut, df1=(2-1), df2=(n-2), ncp = delta)
}

powerf = numeric(length(BETA1))
for  ( j in seq_along(BETA1) ) powerf[j] = Kf(beta1= BETA1[j], alpha, Xi, sigma)


#===================================================
# Comparing the above two functions
#---------------------------------------------------
plot(NA,NA, xlim=range(BETA1), ylim=c(0,1), type="n",
      xlab=expression(beta[1]), ylab="Power Function")
abline(h=alpha, v=0, col="gold3", lwd=0.5)
lines(BETA1, powerf, lwd=1, col="blue")
lines(BETA1, sim.power, lty=1, col="red")



