## Put comments here that give an overall description of what your
## functions do

## 
## a funtion that will build a cache'able matrix -- where the cache contains
## the inverse of the matrix. "i" is the inverse...
##
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


## 
## this function returns the inverse of the matrix given (matrix must be SQUARE... 
## if the inverse was already calculated the cached inverse is returned, otherwise
## the calculation is performed and that value is cached and returned to the caller
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse = x$getinverse()
  ##
  ## if the cache has something, then return it
  ##
  if( !is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ##
  ## calculate the inverse of a SQUARE matrix and return it.
  ##
  data = x$get()
  inverse <- solve(data, ...)
  ##
  ## set the cache
  ##
  x$setinverse(inverse)
  inverse
}

