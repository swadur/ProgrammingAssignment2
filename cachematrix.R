## The functions reduces the costful calculation of the inverse of a matrix. It determines if the
## inverse of an unchanged matrix have previously been calculate, and returns the cached result.
## Otherwise, it calculates and returns the inverse, while caching it for future use.

## Write a short comment describing this function
## This function creates a special vector, by taking a matrix argument, which is a list containing 4 functions:
## get function: returns the matrix argument.
## set function: assigns values to object variables using <<- operator
## setInverse function: uses solve function to determine the inverse of a given matrix. This inverse is assigned to variable m.
## getInverse function: returns the value of m, which is the inverse of a given matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## This function calculates the inverse of the special vector (created by makeCacheMatrix).
## It first checks if the inverse has previously been calculated, and matrix has not changed. If yes, then it returns
## the cached inverse. Otherwise, it calculates and returns the inverse.

cacheSolve <- function(x, ...) {
  
  ## Returns the second argument of the list x, created from makeCacheMatrix. This value is NULL if the inverse has not been calculated for the matrix yet.
  m <- x$getInverse() 
  
  ## Returns the value of m if it is not NULL and exits the function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Otherwise, calculate its inverse using solve and return the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
