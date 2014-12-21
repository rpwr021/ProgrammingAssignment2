## Put comments here that give an overall description of what your
## functions do
## submisiion rpwr021

## create matrixe which can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
        set <- function(y) {

                x <<- y
                m <<- NULL
                            }

        get <- function() x
        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m
                list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should return then
# inverse from the cache.

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            m <- x$getinv()
        if(!is.null(m)) {
                            message("Retrieving Cached Data ")
                        return(m)
                                }
                data <- x$get()
                m <- solve(data) %*% data
                        x$setinv(m)
                        m
}
