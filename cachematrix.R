## Creates a function with a matrix object that caches and stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inversefun) inverse <<- inversefun
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##Creates a function to compute the inverse of the matrix created in the previous
## function. If the inverse has been calculated, it simply retrieves the inverse from the
## cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}