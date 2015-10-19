## Put comments here that give an overall description of what your
## functions do

## This defines the methods for the makeCacheMatrix object



makeCacheMatrix <- function(x = matrix()) {
# Creates a special "matrix", which is really a list containing a function to: 
#  o set the value of the matrix
#  o get the value of the matrix
#  o set the value of the inverse of the matrix
#  o get the value of the inverse of the matrix

  # Initialize inverse to Null
  i <- NULL
  
  # Function to set value of x:
  set <- function(y) {
  # Note use of <<- to set in parent environment
    x <<- y 
	# Reset Inverse
    i <<- NULL
  }
  
  # Function to return value of x:
  get <- function() x
  
  # Function to set value of i: 
  # Note use of <<- to set in parent environment
  setinverse <- function(inverse) i <<- inverse
  
  # Function to return value of i: 
  getinverse <- function() i
  
  # Return the list of functions: 
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
# Calculate the inverse of the "matrix". First check if inverse has already been calculated. 
# If so set the inverse from the cache and skip the calculation
# Othereise calculate the inverse and set/cache the result

  ## Use function defined above: 
  i <- x$getinverse()
  
  # If this returns a value then return it
  if (!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  
  # Otherwise get the data ...
  data <- x$get()
  
  # ... and calculate the inverse: 
  i <- solve(data, ...)
  
  # Now set/cache this result: 
  x$setinverse(i)
  i
}
