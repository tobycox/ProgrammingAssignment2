## Functions to calculate the inverse of a matrix
## using cacheing to avoid repeat calulations
## 
## Example usage:
## v <- matrix(c(1,2,3,4),ncol=2)
## mat <- makeCacheMatrix()
## mat$set(v)
## cacheSolve(mat)

## Returns a hold the input and cached value
## to be used by the 'cacheSolve' function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a list created by the 'makeCacheMatrix' function
## and returns the inverse (using a cached value if available)
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    message("calculating new value")
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}