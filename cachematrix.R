## Provides a enchanced version of a matrix that is able to cache the inverse of a matrix.
## Since computing the inverse of a matrix can be a costly operation, caching the inverse  
## provides a performance benefit.  The inverse is only cached if the matrix has not changed. 
## Once the matrix has changed, the cache is cleared and inverse is calculate again.

## A special version of a matrix stores a cached inverse of the matrix.
## Functions provide the ability to set/get the matrix and 
## setinverse/getinverse the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix x.  x is a makeCacheMatrix.  If the 
## matrix has a cache inverse it is returned.  Otherwise the inverse is calculate using the
## the solve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
