## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list containing the following functions:
##     set: set the value of the matrix and clear the inverse matrix value
##     get: get the value of the matrix
##     setinverse: ASIGN a the inverse matrix value (It doesn't calculates it)
##     getinverse: gets the stored inverse matrix value 

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


## Write a short comment describing this function
## Attempts to find the inverse of a matrix in the cache, otherwise
##   calculates it and stores it in cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {     #attemp to find the inverse in cache
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # calculate the inverse
    x$setinverse(m)       # store it in cache
    m
    ## Return a matrix that is the inverse of 'x'
}