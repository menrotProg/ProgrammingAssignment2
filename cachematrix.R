## Assignment #2
## As matrix inversion is a costly computation, there may be some benefit to caching the inverse of a matrix rather than 
##  compute it repeatedly. We define a pair of functions that cache the inverse of a matrix. 
##  The makecacheMatrix() function creates the structure for the cache and the associated functions to handle it.
##  The cacheSolve() function checks if the inverse is stored in the cache and returns it if it exists, 
##  or calculates the inverse, if the cache is not valid.


## This function creates a cache to store the inverse of the matrix x, and the functions to use access 
##   the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## X is a matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function creates an inverted matrix if not cached already. If the cache is valid, is skips the calculation and
##   returns the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


