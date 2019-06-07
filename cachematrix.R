## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly
## This program contains a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

x <- rbind(c(10,1), c(1,10))

makeCacheMatrix <- function(x = matrix()) {
    solx <- NULL
    set <- function(y) {
        x <<- y
        solx <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) solx <<- solve
    getsolve <- function() solx
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    solx <- x$getsolve()
    
    if (!is.null(solx)) {
        message("Cached data retrieved")
        return(solx)
    }
    data <- x$get()
    solx <- solve(data)
    x$setsolve(solx)
    solx
}
