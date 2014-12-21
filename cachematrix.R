## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x=matrix()){
        m <- NULL
    set <- function(y){                 ##set the value of the matrix
            x<<-y
            m<<-NULL
        }
        get <-function() x              ##get the value of the matrix
            setinv <- function(inv) m <<-inv        ##set the value of the inverse
            getinv <- function() m              ##get the valeur of the inverse
                list(set=set,get=get,               ##give the list of accessible defined functions from outside
                            setinv = setinv,
                                    getinv = getinv)
}

cacheSolve <- function (x,...){
        m <-x$getinv() ## call to the getinv function from makeCacheMatrix to check if m already exist (m=inverse value of matrix)
    if(!is.null(m)){
                message("getting cached data")
            return(m)  ##m has already been calculated, so we can return the value of m, and terminate as return
                }
        data <-x$get() ##get fom makeCacheMatrix the value of the matrix to be inversed
            m<-solve(data,...) ##inverse the matrix
            x$setinv(m) ##set the matrix in makeCacheMatrix
                m
}
