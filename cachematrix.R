## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then  
## cacheSolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 i <- NULL 
  set <- function(y) { ## set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x ## get the value of the matrix
  setinverse <- function(inverse) i <<- inverse ## set the value of the inverse
  getinverse <- function() i ## get the value of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse() # get the inverse
  if(!is.null(i)) {  # return the inverse if it is not null
    message("getting cached data")
    return(i)
  }
  # otherwise compute the inverse
  data <- x$get()
  i <- solve(data) 
  x$setinverse(i)
  i
}
