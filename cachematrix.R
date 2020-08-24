## These functions have been developed as part of a Coursera R Programming course
## assignment. 

## Creates a set of utility functions to work with a matrix and cache the results of
## inverting it. It's assumed the matrix is  always invertible.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Inverts a matrix, but caches the result for subsequent invocations. 
## x should be a list of functions as returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  s
}
