require(arm)
require(MASS)
require(dplyr)
require(broom)

fsample <- function(firmsurv = 200, rate = .5, sd = 1, b = c(0, 1, 1, 1)
                  , d = c(.5, .5)){
    ## http://www.r-bloggers.com/how-to-write-and-debug-an-r-function/
    ## To add checks on the inputs
    firms <- firmsurv/rate
    sigma <- matrix(c(.5, 0, 0, .5), nrow = 2)
    x2 <- mvrnorm(firms, c(0,0), sigma)
    x = cbind(1, x2[, 1], x2[, 2], x2[, 1]*x2[, 2])
    yhat = x %*% b - d[1] * x2[,1]^2 - d[2] * x2[,2]^2
    y = rnorm(yhat, yhat, sd)

    ## stupid selection mechanism
    ## select the rate highest percentile, can probably be improvemed
    ## by working with a probabilitstic model.
    ## Only improvement when directly modeling the censoring?
    survive <- I(y > quantile(y, (1-rate)))
    xsurv <- x[survive, ]
    ysurv <- y[survive]
    dt <-  tbl_df(as.data.frame(xsurv)) %>%
        rename(ct = V1, x1 = V2 , x2 = V3, x1x2 = V4) %>%
        mutate(y = ysurv)
    return(dt)
}

fmatch <- function(dt){
    reg <- lm(x1 ~ x2, data = dt)
    results <- tidy(reg) %>%
        filter(term == "x2") %>%
        rename(test = term) %>%
        mutate(r.squared = select(glance(reg), r.squared)[1,1])
    results[1, 1] <- "match"
    return(results)
}

fprofit <- function(dt){
    reg <- lm(y ~ x1*x2, data = dt)
    results <- tidy(reg) %>%
        filter(term == "x1:x2") %>%
        rename(test = term) %>%
        mutate(r.squared = select(glance(reg), r.squared)[1,1])
    results[1, 1] <- "profit"
    return(results)
}

fsimulate <- function(..., nsim = 100){
    dt <- tbl_df(data_frame())
    for (i in 1:nsim){
        dtsample <- fsample(...)
        dt <- dt %>% bind_rows(fmatch(dtsample)) %>%
            bind_rows(fprofit(dtsample))
    }
    return(dt)
}

frun <- function(firmsurv = 200, rate = .5, sd = 1, b = c(0, 1, 1, 1),
                      d = c(0.5, 0.5), nsim = 100, seed = 12345){
    set.seed(seed)
    matb <- matrix(b, ncol = 4)
    matd <- matrix(d, ncol = 2)
    nsd <- length(sd)
    nb <- length(b)/4
    nd <- length(d)/2
    dt <- tbl_df(data.frame())
    for (i in 1:nsd){
        for (j in 1:nb){
            for (k in 1:nd){
                dt_ <- fsimulate(firmsurv = firmsurv, rate = rate,
                                sd = sd[i], b = matb[j,], d = matd[k,],
                                nsim = nsim) %>%
                    mutate(par.sd = sd[i], par.b1 = matb[j, 1],
                           par.b2 = matb[j, 2], par.b3 = matb[j, 3],
                           par.b4 = matb[j, 4], par.d1 = matd[k, 1],
                           par.d2 = matb[k, 2])
                dt <- bind_rows(dt, dt_)
            }
        }
    }
    return(dt)
}

nsim <- 200
simulation1 <-  replicate(nsim, foutput())
simulation2 <-  replicate(nsim, foutput(sd = 3))
simulation3 <-  replicate(nsim, foutput(d = c(2, 2)))
simulation4 <-  replicate(nsim, foutput(sd = 3, d = c(2,2)))

simullist <- list(s1 = simulation1, s2 = simulation2, s3 = simulation3,
                  s4 = simulation4)

stat <- 8 #see foutput return
names <- rep(names(simullist), each = nsim)
statistics <- c()
for (l in simullist){
    statistics <- c(statistics, l[stat,])
}
boxplot(statistics ~ names)







sim <- simulation4
mean(sim[1,])
sd(sim[1,])
sum(I(sim[5,] < .05) & sim[6,] > 0)/length(sim[5,])
mean(sim[2,])
sd(sim[2,])
sum(I(sim[6,] < .05) & sim[6,] > 0)/length(sim[6,])


### tests
s1 <- fsample(rate = .1, b = c(0, 1, 1, 1), d = c(1, 1))
display(s1$profit)
display(s1$match)

### True optimisation
# coefs <- data.frame(b0 = 1, b1 = 1, b2 = 1, b3 = 1, c
# minprofit <- function(coefs, par){
#        with(coefs, - b0 - b1 * par[1] - b2 * par[2]
#                    - b3 * par[1] * par[2]^2 + c1 * par[1]^2 + c2 * par[2]^2)
#    }
## Add disturbances to the x's
