
## Function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # Set Inverse function
  setInverse <- function(solve) m <<- solve
  ## Get inverse function
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cache <- x$getInverse()
  ## If cache is not empty then getting data from cache
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  ## If cache is empty then calculate Solve and add it to the cache
  my_matrix <- x$get()
  cache <- solve(my_matrix, ...)
  x$setInverse(cache)
  cache
}
