
# Note: Drill5 will be helpful for solving Drill7. 

#===================================================
pdf(file="z-chisq-t-F-test.pdf", width=5.5, height=6.0)
par(mfrow=c(1,1), mar=c(5, 5, 1, 1), omi=c(0,0,0,0), cex=0.6, mex=0.6)
#---------------------------------------------------


ITER = 500
sigma = 5
n = 10
alpha = 0.05
Xi = 1:n

BETA1 = seq(-2,2, by=0.05)

#===================================================
# Simulated power
#===================================================

#---------------------------------------------------
# sigma: known
#---------------------------------------------------
# z-test 
sim.powerz = numeric(length(BETA1))
for ( j in seq_along(BETA1) ) { 
    beta0 = runif(1)                      # any number
    for ( iter in seq_len(ITER) ) {
        ei = rnorm(n, mean=0, sd=sigma)   # N(0,sigma^2) <--- normal only
        Yi = beta0 + BETA1[j]*Xi + ei

        Xbar = mean(Xi)
        Ybar = mean(Yi)
        Sxy = sum(Xi*Yi) - n*Xbar*Ybar
        Sxx = sum(Xi^2) - n*Xbar^2
        
        b1hat = Sxy / Sxx
        b0hat = Ybar - b1hat*Xbar
        
        Yihat = b0hat + b1hat*Xi
        zstat = b1hat / sqrt(sigma^2/Sxx)
        z.cut = qnorm(1-alpha/2)
        if ( abs(zstat) > z.cut )  sim.powerz[j] = sim.powerz[j] + 1/ITER
    }
}

# chisq-test 
sim.powerchisq = numeric(length(BETA1))
for ( j in seq_along(BETA1) ) { 
    beta0 = runif(1)                      # any number
    for ( iter in seq_len(ITER) ) {
        ei = rnorm(n, mean=0, sd=sigma)   # N(0,sigma^2) <--- normal only
        Yi = beta0 + BETA1[j]*Xi + ei

        Xbar = mean(Xi)
        Ybar = mean(Yi)
        Sxy = sum(Xi*Yi) - n*Xbar*Ybar
        Sxx = sum(Xi^2) - n*Xbar^2

        b1hat = Sxy / Sxx
        b0hat = Ybar - b1hat*Xbar

        Yihat = b0hat + b1hat*Xi
        chisqstat = b1hat^2 / (sigma^2/Sxx)
        chisq.cut = qchisq(1-alpha, df=1)          # 1-alpha is used
        if ( chisqstat > chisq.cut )  sim.powerchisq[j] = sim.powerchisq[j] + 1/ITER
    }
}

#---------------------------------------------------
# sigma: unknown
#---------------------------------------------------
# t-test 
sim.powert = numeric(length(BETA1))
for ( j in seq_along(BETA1) ) { 
    beta0 = runif(1)                      # any number
    for ( iter in seq_len(ITER) ) {
        ei = rnorm(n, mean=0, sd=sigma)   # N(0,sigma^2) <--- normal only
        Yi = beta0 + BETA1[j]*Xi + ei

        Xbar = mean(Xi)
        Ybar = mean(Yi)
        Sxy = sum(Xi*Yi) - n*Xbar*Ybar
        Sxx = sum(Xi^2) - n*Xbar^2

        b1hat = Sxy / Sxx
        b0hat = Ybar - b1hat*Xbar

        Yihat = b0hat + b1hat*Xi
        MSE = sum( (Yi - Yihat)^2 ) / (n-2)
        tstat = b1hat / sqrt(MSE/Sxx)
        t.cut = qt(1-alpha/2, df=(n-2) )
        if ( abs(tstat) > t.cut )  sim.powert[j] = sim.powert[j] + 1/ITER
    }
}

# F-test 
sim.powerf = numeric(length(BETA1))
for ( j in seq_along(BETA1) ) {
    beta0 = runif(1)                      # any number
    for ( iter in seq_len(ITER) ) {
        ei = rnorm(n, mean=0, sd=sigma)   # N(0,sigma^2) <--- normal only
        Yi = beta0 + BETA1[j]*Xi + ei

        Xbar = mean(Xi)
        Ybar = mean(Yi)
        Sxy = sum(Xi*Yi) - n*Xbar*Ybar
        Sxx = sum(Xi^2) - n*Xbar^2

        b1hat = Sxy / Sxx
        b0hat = Ybar - b1hat*Xbar

        Yihat = b0hat + b1hat*Xi
        MSR = sum( (Yihat-Ybar)^2 ) / (2-1)
        MSE = sum( (Yi - Yihat)^2 ) / (n-2)
        Fstat = MSR / MSE
        F.cut = qf(1-alpha, df1=(2-1), df2=(n-2) )  # 1-alpha is used
        if ( Fstat > F.cut )  sim.powerf[j] = sim.powerf[j] + 1/ITER
    }
}

#===================================================
# Theoretical power
#===================================================

#---------------------------------------------------
# sigma known
#---------------------------------------------------
# z-test 
Kz = function(beta1, alpha, Xi, sigma) { 
     n = length(Xi)
     Xbar = mean(Xi)
     Sxx = sum(Xi^2) - n*Xbar^2
     tmp = beta1 / (sigma/sqrt(Sxx))
     z.cut = qnorm(1-alpha/2 )
     pnorm(z.cut+tmp, lower.tail=FALSE) + pnorm(-z.cut+tmp)
}
powerz = numeric(length(BETA1))
for  ( j in seq_along(BETA1) ) powerz[j] = Kz(beta1= BETA1[j], alpha, Xi, sigma)


# chisq-test 
Kchisq = function(beta1, alpha, Xi, sigma) { 
     n = length(Xi)
     Xbar = mean(Xi)
     Sxx = sum(Xi^2) - n*Xbar^2
     delta = beta1^2*Sxx / sigma^2
     chisq.cut = qchisq(1-alpha, df=(2-1) )
     1-pchisq(chisq.cut, df=(2-1), ncp = delta)
}
powerchisq = numeric(length(BETA1))
for  ( j in seq_along(BETA1) ) powerchisq[j] = Kchisq(beta1=BETA1[j], alpha, Xi, sigma)


#---------------------------------------------------
# sigma unknown
#---------------------------------------------------
# t-test 
Kt = function(beta1, alpha, Xi,sigma) { 
     n = length(Xi)
     Xbar = mean(Xi)
     Sxx = sum(Xi^2) - n*Xbar^2
     ncp = beta1 / (sigma/sqrt(Sxx))
     t.cut = qt(1-alpha/2, df=(n-2) )
     pt(t.cut,df=n-2,ncp=ncp, lower.tail=FALSE) + pt(-t.cut,df=n-2,ncp=ncp)
}
powert = numeric(length(BETA1))
for  ( j in seq_along(BETA1) ) powert[j] = Kt(beta1= BETA1[j], alpha, Xi, sigma)

# F-test 
# Later 




#===================================================
# Plots of the above functions and simulations
#---------------------------------------------------
plot(NA,NA, xlim=range(BETA1),  ylim=c(0,1), type="n",
      xlab=expression(beta[1]), ylab="Power Function")
abline(h=alpha, v=0, col="gold3", lwd=0.5)

#--------------------
# Simulated powers
#--------------------
# sigma: known
# z-test
lines(BETA1, sim.powerz,     lty=2, col="black", lwd=0.8)
# chisq-test
lines(BETA1, sim.powerchisq, lty=1, col="cyan3", lwd=0.7)

# sigma: unknown
# t-test
lines(BETA1, sim.powert, lty=2, lwd=0.8, col="red3")
# F-test
lines(BETA1, sim.powerf, lty=1, lwd=0.8, col="pink")

#--------------------
# Theoretical powers
#--------------------
# sigma: known
# z-test
 lines(BETA1, powerz,     lty=1,  col="black")
# chisq-test
points(BETA1, powerchisq, pch=20, col="cyan3", cex=0.8)

# sigma: unknown
# t-test
 lines(BETA1, powert,     lty=1,  lwd=1.0, col="red3")
# F-test
# LATER 


#--------------------
# legends
#--------------------
# sigma: known
# z-test and chisq-test (theoretical)
legend (-1.70,  1.02, legend=c(expression(K[z](beta[1])), expression(K[chi^2](beta[1]))),
        horiz=FALSE, bty="n", lty=c(1,NA), pch=c(NA,20), lwd=0.7, cex=0.8, col=c("black","cyan3") )

# z-test and chisq-test (simulation)
legend (-1.75,  0.97, legend=c("Simulated Power (z-test)", "Simulated Power (chisq-test)"),  cex=0.8, lwd=0.8,
        horiz=FALSE, bty="n", lty=c(2,1), col=c("black","cyan3") )

# sigma: unknown
# t-test and F-test (theoretical)
legend (-1.70,  0.20, legend=c(expression(K[t](beta[1])), expression( paste(K[F](beta[1]), ": not yet"))),
        horiz=FALSE, bty="n", lty=c(1,NA), pch=c(NA,20), lwd=0.8, cex=0.8, col=c("red3","pink2") )

# t-test and F-test (simulation)
legend (-1.75,  0.15, legend=c("Simulated Power (t-test)", "Simulated Power (F-test)"),
        horiz=FALSE, bty="n", lty=c(2,1), col=c("red3","pink2"), lwd=0.8, cex=0.8 )

# alpha (significance level)
text(-1.0, 0.035, labels=expression(alpha==0.05))
