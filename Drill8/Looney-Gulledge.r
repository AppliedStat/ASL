#-------------------------------------------------------------
# Table 2. (Looney and Gulledge, 1985)
#-------------------------------------------------------------
iter = 10000

## n = 100
n = 5

COR = numeric(iter)

for ( i in seq_len(iter) ) {
    z = sort(rnorm(n))
    q = qnorm(ppoints(n))
    COR[i] = cor(z,q)
}

plot(density(COR))

levels = c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995)
c(min(COR), quantile(COR, prob=levels, type=5))

#-------------------------------------------------------------
# HW: Can you improve the above R program by avoiding for() loop.
