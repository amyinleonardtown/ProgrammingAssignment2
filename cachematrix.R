## These functions create a matrix and solve for the inverse matrix, storing
## the inverse for future reference.

## This function creates a list of functions to set and get the value of
## a matrix and the value of its inverse.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matrixinv) inv <<- matrixinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function uses the above function to calculate the inverse of a matrix.
## In the event that the inverse has already been calculated, it calls up that 
## value from the cache.  If the inverse has not been calculated, the function
## uses the above to calculate the inverse and store in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
