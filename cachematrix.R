## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property as NULL
  
  # Set the matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Set the inverse of the matrix in the cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse, if it exists
  if(!is.null(inv)) {    # If there is a cached value, use it
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, get the matrix, compute its inverse, and cache the result
  data <- x$get()
  inv <- solve(data, ...)  # Calculate the inverse of the matrix
  x$setInverse(inv)        # Cache the inverse
  inv                      # Return the inverse
}
