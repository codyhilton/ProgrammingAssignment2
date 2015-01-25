## These functions make use of the lexical scoping rules in R. The function
## makeCacheMatrix creates a special matrix and can cache its inverse.
## cacheSolve is a function that takes the matrix created by makeCacheMatrix and 
## checks to see if the inverse is in cache. If the inverse is in cache then it 
## returns the cached value. If not then it computes the inverse and returns that value.


## makeCacheMatrix is a function that creates a special matrix and returns four
## functions in a list. The four functions set, get, set the inverse, and get the
## inverse of the matrix.

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


## cacheSolve is a function that works with the function makeCacheMatrix. makeCacheMatrix
## checks to see the inverse of the matrix is in cache. If it is in cache the value is printed 
## to the console if not the inverse is calculated, stored in cache, and printed to the console

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
