## Assignment: Caching the Inverse of a Matrix
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixinverse <<- solve
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
