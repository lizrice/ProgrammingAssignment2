## Return the inverse of a matrix, caching the result for efficiency 

## Creates a "matrix" object that can store a cache of its inverse
makeCacheMatrix <- function(x = matrix()) {
  # We don't know the inverse to start with
  inv <- NULL 
  
  # When we have a new matrix, store it and reset the inverse. (We don't
  # calculate the inverse until we need it.)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Return the original matrix
  get <- function() x                             
  
  # Cache the inverse
  setinverse <- function(inverse) inv <<- inverse 
  
  # Return the cached inverse 
  getinverse <- function() inv                    
  
  # Return a list of the functions 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()   # Do we have a cached version of the inverse?
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)           # Yes, this is the cached version of the inverse
  }
  data <- x$get()         # We didn't have a cache of the inverse so get the original matrix ...
  inv <- solve(data, ...) # ... find its inverse 
  x$setinverse(inv)       # ... and cache it
  inv                     # Return the inverse
}
