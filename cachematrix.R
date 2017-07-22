## The following functions are provided:

## 1. makeCacheMatrix which creates a set of functions to solve and
##    cache the inverse of a matrix
## 2. cacheSolve which uses the functions returned by makeCacheMatrix to 
##    find the inverse of a matrix, or to use the cached inverse if it exists

## makeCacheMatrix takes a matrix as input and creates
## a set of functions to solve and cache the inverse of that matrix, returning 
## these functions in a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setinv <- function(inv) inv <<-solve
  getinv <- function() inv
  list(get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes the set of functions returned by makeCacheMatrix
## and executes them to return the inverse.
## It first gets the cached value and checks if it is null, if not null, 
## this value is returned, otherwise the inverse is calculated, cached and
## then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
