## Assignment #2
## The makecacheMatrix function create a matrix received as input x, and associated functions
##  that set the matrix, get the matrix and sets and gets the inverted matrix, using the solve() 
##  function

## This function creates a cached copy of the matrix x, and the functions to use it

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


## This function creates and inverted matrix if not cached already

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


