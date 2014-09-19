## This file contains 2 functions that can be used to store an invertible matrix 
## as a special object, then calculate and cache the inverse matrix


## makeCacheMatrix creates a special matrix object that is capable of caching 
## it's inverse. It defines 4 functions as part of the matrix object.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(mx) inv <<- mx
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse matrix unless it is already cached in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mx <- x$get()
    inv <- solve(mx)
    x$setInverse(inv)
    inv
}
