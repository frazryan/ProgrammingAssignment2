## This function generates a special matrix
## with metadata to control logic for caching
## the inverse.
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL ## reset x_inv to NULL if matrix changes
  }
  get <- function() x
  setinv <- function(x_inv_in) x_inv <<- x_inv_in
  getinv <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the cached inverse if it
## exists and if the data has not changed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
