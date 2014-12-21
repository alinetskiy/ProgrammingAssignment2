## The functions to use caching with calculations of matrix inverse. 


## Creates a "matrix" which is capable of caching its inverse. The "matrix" is actually 
## a list with 4 functions:
##
## - set - set the value of matrix
## - get - returns the value of matrix
## - setInverse - caches the calculated matrix inverse
## - getInverse - returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of the "matrix". If possible, uses the cached value. Otherwise, 
## calculates the inverse and caches it.
## Arguments:
## - x - "matrix" , created with makeCacheMatrix function
## ... - other parameters to pass into solve function for calculating inverse. Please note that 
##       if the cached value is used, the other parameters are ignored. 

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv) 
    inv    
}
