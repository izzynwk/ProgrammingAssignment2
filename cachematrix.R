## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a matrix object to cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the matrix returned by `makeCacheMatrix` above. 
## When the inverse is calculated then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mx, ...) {
  inverse <- mx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mx$get()
  invserse <- solve(data, ...)
  mx$setinv(inverse)
  return(inverse)
}