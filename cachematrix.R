## These functions return the inverse of an invertible matrix
## A new matrix is processed and its inverse (and the matrix itself) is cahed.
## Later, the inverse is accessed in the cache instead of re-calculating it.

## This function results four function in a list to assess storing a matrix and its inverse.
## Using *$set() a matrix can be defined of which inverse should be computed and stored
## Using *$get() returns the stored matrix
## Using *$setinverse() the inverse will be stored; NOTE: Should be accessed via 'cacheSolve', not directly
## Using *$getinverse() returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of an invertible matrix stored in object 'x'.
## First, the cahce is searched for the result. A message is also printed this case
## to mention that the displayed inverse is drawn from the cache.
## If the input matrix and its inverse is not cached, the function calculates the inverse
## using the solve function and sets .

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
