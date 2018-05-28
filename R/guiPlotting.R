plotlyModelPlot <- function(model,input, which = 1:2,
                            constant = model$x[which.min(model$y),]
                            ,xlab = paste("x", which[1], sep = ""),
                            ylab = paste("x", which[2], sep = ""),
                            amntPointsPerDimension = 15){
    fun <- evaluateModel(model)

    lower <- apply(model$x[, which], 2, min)
    upper <- apply(model$x[, which], 2, max)

    x <- seq(lower[1], upper[1], length = amntPointsPerDimension)
    y <- seq(lower[2], upper[2], length = amntPointsPerDimension)

    nvar <- ncol(model$x)
    vary <- (1:nvar) %in% which

    if (nvar > 2) {
        plotfun2 <- fun
        fun <- function(xx) {
            z2 <- matrix(constant, nrow(xx), nvar, byrow = TRUE)
            z2[, which(vary)[1]] <- xx[, 1]
            z2[, which(vary)[2]] <- xx[, 2]
            plotfun2(z2)
        }
    }

    if (length(formals(fun)) == 1){
        fn <- function(a, b) {
            fun(cbind(a, b))
        }
        z <- outer(x, y, fn)
    }else if (length(formals(fun)) == 2){
        z <- outer(x, y, fun)
    }
    p <- plot_ly(z = ~t(z), x = x, y = y) %>% add_surface()
    p$elementId <- NULL
    return(p)
}
