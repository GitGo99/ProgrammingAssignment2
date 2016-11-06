## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly.
##The following are two functions that cache and compute the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y;
    inv <<- NULL;
  }
  get <- function() x;
  setinv <- function(inverse) inv <<- inverse;
  getinv <- function() inv;
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special
## "matrix" created with the above function. It first check to see whether the inverse has
## already been calculated. If so, it gets the inverse from the cashe and skips the computation. 
## Otherwise it calculates the inverse of the maxtrix and sets the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


