## These functions allows us to work with a different object than a matrix,
## that contains the matrix and its inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the matrix located in the
##             special object descibed above just in the case when its inverse
##             is not already computed inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinverse(inverse)
  inverse
}
