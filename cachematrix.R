## These functions work together to calculate and cache the inverse of a
## given matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its
## inverse. It provides getter/setter functions for the matrix and its
## computed inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  # External getter/setter functions.
  set <- function(new_matrix) {
    x <<- new_matrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(new_inverse) inverse <<- new_inverse
  getinverse <- function() inverse
  
  # Return the external functions.
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been computed, it uses the
## cached version.
cacheSolve <- function(x, ...) {
  # Use the cached inverse if it exists.
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("using cached inverse")
    return(inverse)
  }

  # Otherwise, compute and return the inverse.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
