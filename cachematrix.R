## This pair of functions allows to manage matrices
## that optimize complex and time consuming calculations
## by caching a previous calculated result.


## makeCacheMatrix converts a matrix into an object
## that can hold previously calculated results.
## Usage:
##   mcm <- makeCacheMatrix(a_matrix)
##   mcm$get() returns the original a_matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the matrix
## It's parameter must be of the type constructed
## by makeCacheMatrix. If the inverse of that
## matrix was already calculated it returns this
## results. Otherwise it calls the R function solve
## and store the result for later reuse.
## Usage:
##    inverse <- cacheSolve(mcm)

cacheSolve <- function(x, ...) {
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
