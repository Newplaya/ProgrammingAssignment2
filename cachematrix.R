## Functions to cache and inverse a matrix

## Creating a cache for the matrix and the inverted matrix
## pretty much the same as in the makevector example
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(matrix){
    x <<- matrix
    inv <<- NULL
    
  }
  
  get <- function () x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Check if the inverse of the matrix is already in cache and if not compute it

cacheSolve <- function(x, ...) {
  ## get the inverse
  m <- x$getinverse()
  
  ## if the inverse is not null (so there is data in m) print it out from the cache
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix from the cache
  data <- x$get()
  
  ## put the inverse in m
  m <- solve(data,...)
  
  ## set the inverse to m so we can check later again if the inverse is calculated already
  x$setinverse(m)
  
  m
}
