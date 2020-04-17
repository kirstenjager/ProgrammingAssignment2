## The following functions first create a matrix and then compute the reverse.
## It does so by caching the inverse. If the inverse has already been calculated and not changed, then 
## the inverse is retrieved from the cache.

## makeCacheMatrix creates a matrix object that can cache its reverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of x. If the inverse has already been calculated, it retrieves
## this inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
}
