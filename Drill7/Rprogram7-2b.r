
# Note: Drill5 will be helpful for solving Drill7. 

#===================================================
 pdf(file="t-F-test.pdf", width=5.5, height=3.0)
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
# t-test (sigma: unknown)
sim.powert = numeric(length(BETA1))
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
        MSE = sum( (Yi - Yihat)^2 ) / (n-2)
        tstat = b1hat / sqrt(MSE/Sxx)
        t.cut = qt(1-alpha/2, df=(n-2) )
        if ( abs(tstat) > t.cut )  sim.powert[j] = sim.powert[j] + 1/ITER
    }
}

# F-test (sigma: unknown)
sim.powerf = numeric(length(BETA1))
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
        if ( Fstat > F.cut )  sim.powerf[j] = sim.powerf[j] + 1/ITER
    }
}



#===================================================
# Theoretical power
#---------------------------------------------------
Kt = function(beta1, alpha, Xi,sigma) { 
     n = length(Xi)
     Xbar = mean(Xi)
     Sxx =  (sum(Xi^2)-n*Xbar^2)
     ncp = beta1 / (sigma/sqrt(Sxx))
     t.cut = qt(1-alpha/2, df=(n-2) )
     pt(t.cut,df=n-2,ncp=ncp, lower.tail=FALSE) + pt(-t.cut,df=n-2,ncp=ncp)
}
powert = numeric(length(BETA1))
for  ( j in seq_along(BETA1) ) powert[j] = Kt(beta1= BETA1[j], alpha, Xi, sigma)


#===================================================
# Comparing the simulated t-test and F-test with theoretical t-test
#---------------------------------------------------
plot(NA,NA, xlim=range(BETA1), ylim=c(0,1), type="n",
      xlab=expression(beta[1]), ylab="Power Function")
abline(h=alpha, v=0, col="gold3", lwd=0.5)

lines(BETA1, powert,     lty=1, col="pink3", lwd=1.0)
lines(BETA1, sim.powert, lty=2, col="red3")
lines(BETA1, sim.powerf, lty=2, col="blue")

legend (-1.0,  1.02, legend=c( expression(K[t](beta[1])) ),
        horiz=FALSE, bty="n", lty=c(1), lwd=0.8, col=c("pink3") )
legend (-0.01, 1.02, legend=c("Simulated Power (F-test)", "Simulated Power (t-test)"),  
        horiz=FALSE, bty="n", lty=c(2,2), col=c("blue", "red3") )

