## This function creates a special Matrix
## Create makecachematrix Function
makecachematrix <- function(x = matrix()) {
  ## Create a special objects that stores a matrix and cache its inverse
  inv1 <- NULL
  set1 <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  get <- function() x
  setinv1 <- function(solvematrix) inv1 <<- solvematrix
  getinv1 <- function() inv1
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is compute the inverse of the special matrix.
cachesolve <- function(x, ...) {
  ## Return the matrix that is the inverse of x
  inv1 <- x$getinverse()
  if(!is.null(inv1)) {
    message("Getting Cache Data")
    return(inv1)
  }
  mat1 <- x$get()
  inv1 <- solve(mat1, ...)
  x$setinverse(inv1)
  inv1
}
