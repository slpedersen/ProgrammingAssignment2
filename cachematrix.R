## R Programming
## Peer Assignment

## Pair of functions to support caching the inverse of a matrix
##
## makeCacheMatrix()
##    input - x, where x is a matrix
##    output - a list of four functions (get(), set(), getinverse(), setinverse())
##            that acts as an object-oriented matrix that can cache its own inverse
##
## cacheSolve()
##    input - xx, where xx is output of the makeCacheMatrix() function
##    output - a matrix which is the inverse of xx

## makeCacheMatrix(x) - create an object-oriented list which acts
##                      as an inverse-caching version of the input matrix, x

makeCacheMatrix <- function(x = matrix()) {
        ## Create a list of four-functions that acts as an
        ## object-oriented matrix that can cache its own inverse
        m <- NULL
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve(xx) - calculate and cache the inverse of the special "matrix" xx
##                  Here xx is the output of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## and cache the result
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
