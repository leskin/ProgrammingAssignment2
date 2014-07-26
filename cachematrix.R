## The makeCacheMatrix() and cacheSolve() are used to create a special matrix object, and to
## calculate and return the inverse of a matrix.  The first time that CacheSolve() is called
## on a matrix object, the matrix inverse is calculated and stored in the special matrix object.
## Subsequent times that cacheSolve() is called on a special matrix object, the previously-calculated
## matrix inverse is simply read from the special matrix object and returned.

## The function makeCacheMatrix creates and returns a special matrix object.
## The special matrix object is a list object with four components which are functions to:
## 1) get the matrix
## 2) set the matrix
## 3) get the matrix inverse
## 4) set the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mat_inv <<- solve
  getInverse <- function() mat_inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve returns the inverse of a matrix. The matrix is supplied
## in a special matrix object that is created by the function makeCacheMatrix.
##
## 1) Before calculating the matrix inverse, cacheSolve checks to see if the special matrix object
##    already contains the inverse, and if so, it does note repeat the calculation of the inverse
##    but instead retrieves and returns the previously calculated matrix inverse.
## 2) If the matrix object does not contain the matrix inverse, cacheSolve calculates the matrix
##    inverse, stores it in the special matrix object, and returns the calculated matrix inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getInverse()
  if(!is.null(mat_inv)) {
    message("getting matrix inverse from the cache")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setInverse(mat_inv)
  mat_inv
}
