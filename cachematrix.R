## Functions that cash the inverse of a matrix

# Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix
  inverse <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # If the matrix changes, reset the cached inverse
  }
  
  # Getter function to retrieve the matrix
  get <- function() x
  
  # Getter function to retrieve the cached inverse
  getInverse <- function() inverse
  
  # Setter function to cache the inverse
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


# Function to compute the inverse of the "matrix" object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inverse <- x$getInverse()
  
  # If the inverse is not cached, compute it and cache it
  if (!is.null(inverse)) {
    message("Retrieving cached inverse")
    return(inverse)
  }
  
  # Compute the inverse
  mat <- x$get()
  inverse <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(inverse)
  
  # Return the inverse
  inverse
}
