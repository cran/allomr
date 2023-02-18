allomr <- function(X, Y, X0, a, b){
    m1 <- lm(log(Y)~log(X))
    n <- length(X[!is.na(X)])
    if (missing(a)){
      a <- exp(m1$coefficients[1])
    }
    if (missing(b)){
      b <- m1$coefficients[2]
    }
    if (missing(X0)){
      X0 <- mean(X, na.rm=TRUE)
    }
    Yx <- Y*(X0/X)^b
    exp_e <- Y/(a*X^b)
    obj <-list(n, a, b, X0, Yx, exp_e)
    names(obj) <-c("n","a","b","X0","Yx","exp_e")
    return(obj)
}
