## This file contains functions to set and cache the inverse of a matrix

## the makeCacheMatrix function will store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) cached <<- solve
  getInverse <- function() cached
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cachesolve will check to see if the inverse of x is stored, if it is it will retrieve the value from
## the cache, if not it will create and store the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cached <- x$getInverse()
  if(!is.null(cached)){
    message("Getting data from Cache...")
    return(cached)
  }
  inverse <- x$get()
  cached <- solve(inverse, ...)
  x$setInverse(cached)
  cached
}
