## 'makeCacheMatrix' and 'cacheSolve' implement matrix inversion with caching capabilities.
## This allows for skipping time-consuming computations when the inverse is requested
## repeatedly for the same matrix.
##
## It is assumed (and not checked) that the matrix supplied is invertible.


## Function to create a special "matrix" object that can cache its inverse.
## The object is a list containing functions to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function to compute the inverse of the "matrix" object returned by makeCacheMatrix.
## If the inverse is already available, it should retrieve the inverse from the cache
## instead of computing it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
