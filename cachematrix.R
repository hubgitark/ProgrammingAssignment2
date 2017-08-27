# The CACHEMATRIX darkness, which whole point being having a global "x"

# a matrix object that cache its inverse with sort of an API
# no dictionary function
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
  }


# Here cacheSolve takes input from makeCacheMatrix as an argument
# and returns to it
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of the "x" matrix above
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  oriX <- x$get() #the original "x" matrix
  inv <- solve(oriX)
  x$setInverse(inv)
  inv
}

