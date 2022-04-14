## These functions are used to store and recall the values of a 
## matrix and the results of a calculation performed upon it to
## save time. This is like building a class the hard way.

## Creates a matrix object that contains a matrix and its inverse
## in memory. It also provides functions for getting or setting
## those values.
makeCacheMatrix <- function(x = matrix()) {
  # Initializes an object for memory
  m <- NULL
  
  # Stores the matrix and clears the memory
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Retrieves the matrix
  get <- function() x
  
  # Stores the inverse
  setinverse <- function(solve) m <<- solve
  
  # Retrieves the inverse
  getinverse <- function() m
  
  # Returns a list for later calls
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This checks a cached matrix object to see if the matrix has
## already been solved. If it has, then it retrieves the previous
## result. Otherwise it performs the calculation and stores the
## result for a later call.
cacheSolve <- function(x, ...) {
  
  # Tries to retrieve stored inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the inverse is not stored, then retrieves the stored matrix,
  # finds the inverse, then stores it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
