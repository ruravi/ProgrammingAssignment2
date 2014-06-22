## Functions that compute the inverse of a matrix.

## Returns a special internal matrix object that caches the inverse
## for faster lookups.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getInverse <- function() {
    inverse
  }
  
  setInverse <- function(argument) {
    inverse <<- argument
  }
  
  get <- function() {
    x
  }
  list(get = get, getInverse = getInverse, setInverse = setInverse)
}


## Returns the inverse of x, looking up from the cached state or
## caching it if this is the first time the function is called.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  result <- solve(data)
  # Store the result into the object state.
  x$setInverse(result)
  result
}
