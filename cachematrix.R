## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #Function to set value of a matrix. It also clears the inverse.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value of the matrix.
  get <- function() x
  
  # Set the inverse of the matrix.
  setInverse <- function(inv) m <<- inv
  
  # Get the inverse of the matrix.
  getInverse <- function() m
  
  # Return a list of the functions
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


# This function returns the inverse of the matrix.
# If it has already been calculated, then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  
  m<- x$getInverse()
  if(!is.null(m)){
    return (m)
  }
  data <- x$get()  # Get value of the matrix
  m <- solve(data) # Calculate the inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}
