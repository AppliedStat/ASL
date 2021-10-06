
# Note: Drill5 will be helpful for solving Drill7. 

#===================================================
# pdf(file="t-test.pdf", width=5.5, height=3.0)
# par(mfrow=c(1,1), mar=c(4, 4, 1, 1), omi=c(0,0,0,0), cex=0.8, mex=0.8)
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
# sigma: unknown
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

# sigma: known
sim.powerz = numeric(length(BETA1))
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
        zstat = b1hat / sqrt(sigma^2/Sxx)
        z.cut = qnorm(1-alpha/2)
        if ( abs(zstat) > z.cut )  sim.powerz[j] = sim.powerz[j] + 1/ITER
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

# sigma known
Kz = function(beta1, alpha, Xi,sigma) { 
     n = length(Xi)
     Xbar = mean(Xi)
     Sxx =  (sum(Xi^2)-n*Xbar^2)
     tmp = beta1 / (sigma/sqrt(Sxx))
     z.cut = qnorm(1-alpha/2 )
     pnorm(z.cut+tmp, lower.tail=FALSE) + pnorm(-z.cut+tmp)
}
powerz = numeric(length(BETA1))
for  ( j in seq_along(BETA1) ) powerz[j] = Kz(beta1= BETA1[j], alpha, Xi, sigma)


#===================================================
# Comparing the above two functions
#---------------------------------------------------
plot(NA,NA, xlim=range(BETA1), ylim=c(0,1), type="n",
      xlab=expression(beta[1]), ylab="Power Function")
abline(h=alpha, v=0, col="gold3", lwd=0.5)

lines(BETA1, powert,     lty=1, col="red3")
lines(BETA1, sim.powert, lty=2, col="red3")

lines(BETA1, powerz,     lty=1, col="black")
lines(BETA1, sim.powerz, lty=2, col="black")

legend (-1.5,  1.02, legend=c(expression(K[z](beta[1])), expression(K[t](beta[1])) ),
        horiz=FALSE, bty="n", lty=c(1,1), col=c("black", "red3") )
legend (-0.01, 1.02, legend=c("Simulated Power (z-test)", "Simulated Power (t-test)"),  
        horiz=FALSE, bty="n", lty=c(2,2), col=c("black", "red3") )

