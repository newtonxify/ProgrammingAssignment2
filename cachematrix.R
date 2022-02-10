## This function creates a matrix "x" object that can cache the actual inverse.
makeCacheMatrix <- function(x = matrix()) {
  # store the actual inverse value
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  # settter for the actual inverse value
  set_inverse <- function(inv) inverse <<- inv
  # getter for the actual inverse value
  get_inverse <- function() inverse
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This function return a matrix that is the inverse of 'x'
##The 'x'is the special matrix
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("aquiring cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}