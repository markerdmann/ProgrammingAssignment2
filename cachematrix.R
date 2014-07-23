## Matrix inversion can be a costly computation, and the two short
## functions below allow you to create a special matrix that can
## cache its inverse and utilize the cache when solving the matrix.

## This function optionally takes a matrix as its argument and
## returns a matrix that can cache its inverse. The following
## methods can be called on the new matrix:
## set: set the matrix data
## get: get the matrix data
## setinverse: set the cached inverse of the matrix
## getinverse: get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL
    set <- function(y) {
        x <<- y
        cached.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cached.inverse <<- inverse
    getinverse <- function() cached.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve acts just like the solve function, but the argument x
## must be a cache matrix returned by makeCacheMatrix. cacheSolve
## will return the cached inverse or find the inverse and cache
## it before returning it.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}