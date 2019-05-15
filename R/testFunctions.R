
#rastrigin test function
funRastrigin <- function(vec)
{
    if(length(dim(vec))<=1){
        sum = 0
        for(i in vec){
            sum = sum + (i^2 - 10 * cos(2*3.1415926*i))
        }
        return(matrix((10*length(vec) + sum), , 1))
    }
    res = matrix(apply(vec,1,"funRastrigin"), , 1)
    return(res)
}

#branin test function
funBranin <- function(vec, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi))
{
    if(length(dim(vec))<=1){
        x1 <- vec[1]
        x2 <- vec[2]
        term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
        term2 <- s*(1-t)*cos(x1)
        y <- term1 + term2 + s - 0.397887
        return(matrix(y, , 1))
    }else{
        return(res = matrix(apply(vec,1,"funBranin"), , 1))
    }
}

#ddmo lecture test function
funDDMO <- function(vec, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi), evalTime = 1)
{
    Sys.sleep(evalTime)
    if(length(dim(vec))<=1){
        x1 <- vec[1]
        x2 <- vec[2]
        term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
        term2 <- s*(1-t)*cos(x1)
        y <- term1 + term2 + s - 0.397887
        return(matrix(y, , 1))
    }else{
        return(res = matrix(apply(vec,1,"funBranin"), , 1))
    }
}
###############
