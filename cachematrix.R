##Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## We assume that the matrix supplied is always invertible.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(inv) invMatrix <<- inv
  getInv <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 invMatrix <- x$getInv()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  x$setInv(invMatrix)
  ## Return a matrix that is the inverse of 'x'
  invMatrix    
}
