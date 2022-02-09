## This function creates a matrix "x" object that can cache the actual inverse.
makeCacheMatrix <- function(x = matrix()) {
  # store the actual inverse value
  act_inverse <- NULL
  set <- function(y) {
    x <<- y
    act_inverse <<- NULL
  }
  
  get <- function() x
  # settter for the actual inverse value
  set_inverse <- function(inv) act_inverse <<- inv
  # getter for the actual inverse value
  get_inverse <- function() act_inverse
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}