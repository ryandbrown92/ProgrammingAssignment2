## makeCacheMatrix creates a list object containing functions for setting the matrix value contained
## in the object, for returning the stored value, for setting the inverse of the matrix and for returning
## this inverse. cacheSolve consumes a CacheMatrix object and returns the inverse of the CacheMatrix.

## makeCacheMatrix consumes a square invertible matrix x and produces a list object whose elements
## are the functions set(y as square matrix), get(), setinverse(z as square matrix), and getinverse().
## set changes the value of the matrix stored in the list object. get returns the value of the stored 
## matrix. setinverse stores the value of the inverse of the stored matrix. getinverse returns the value
## of the stored matrix.

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

## cacheSolve consumes a CacheMatrix object and returns the inverse of the stored matrix. If the inverse
## is not stored in the CacheMatrix object, cacheSolve computes the inverse, stores the inverse in the 
## CacheMatrix, then returns the computed inverse. Otherwise, cacheSolve returns the inverse value stored
## in the CacheObject.

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
