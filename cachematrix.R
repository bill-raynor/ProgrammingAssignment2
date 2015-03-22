## Coursera R Programming 012 - Course Project 2
## W. J. Raynor

## Provides functions to save and retrieve cached inverse of a square matrix, eliminating the
## need for repeated inversions of the same matrix.

## The function makeCacheMatrix() caches/returns the value of a matrix and/or its inverse.
## This is a helper function, called by cacheSolve, to set and retrieve the matrices.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
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


## cacheSolve() returns the inverse of a matrix. If the cached inverse exists, the function retrieves
## and returns that matrix. Otherwise the function solves for the inverse, caches it and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # the inverse does not exist. get it, save it, and return it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
