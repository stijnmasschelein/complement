require(arm)
require(MASS)

fsample <- function(firmsurv = 200, rate = .5, sd = 1, b = c(0, 1, 1, 1)
                  , d = c(.5, .5)){
    ## http://www.r-bloggers.com/how-to-write-and-debug-an-r-function/
    ## To add checks on the inputs
    firms <- firmsurv/rate

    sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
    x2 <- mvrnorm(firms, c(0,0), sigma)
    x = cbind(1, x2[, 1], x2[, 2], x2[, 1]*x2[, 2])
    y = x %*% b - d[1] * x2[,1]^2 - d[2] * x2[,2]^2
    z = rnorm(y, y, sd)

    ## stupid selection mechanism
    ## select the rate highest percentile, can probably be improvemed
    ## by working with a probabilitstic model.
    ## Only improvement when directly modeling the censoring?
    survive <- I(z > quantile(z, (1-rate)))

    xsurv <- x[survive, ]
    zsurv <- z[survive]
    ysurv <- y[survive]

    lmprofit <- lm(zsurv ~ xsurv[,-1])
    lmmatch <- lm(xsurv[,2] ~ xsurv[,3])
    results <- list(profit = lmprofit, match = lmmatch, x = xsurv, z = zsurv, y = ysurv)
    return(results)
}



s1 <- fsample(rate = .1, b = c(0, 1, 1, 1), d = c(1, 1))
display(s1$profit)
display(s1$match)

simulation <-  replicate(100, c(fsample()$match$coefficients[2],
                                fsample()$profit$coefficient[4]))

coefs <- data.frame(b0 = 1, b1 = 1, b2 = 1, b3 = 1, c
minprofit <- function(coefs, par){
        with(coefs, - b0 - b1 * par[1] - b2 * par[2]
                    - b3 * par[1] * par[2]^2 + c1 * par[1]^2 + c2 * par[2]^2)
    }
## Add disturbances to the x's
