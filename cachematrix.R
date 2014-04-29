## Functions that will create and cache matrix inverse objects for lookup at 
## future computations

## Function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #stores inverse matrix
  inverse <- NULL
  
  #sets the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #gets the matrix
  get <- function() x
  
  #sets the inverse 
  setinverse <- function(solve) inverse <<- solve
  
  #gets the inverse
  getinverse <- function() inverse
  
  #returns list with all new functions
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}

## Function searching for cached inverse and otherwise making inverse and storing 
## in cache
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()

  # If the inverse is in cache, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #otherwise calculate and cache the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}