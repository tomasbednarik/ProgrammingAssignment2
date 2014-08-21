## This functions allows you to compute the inverse of a square matrix
## and cache the result - therefore we do not need calculate
## the same costly computation repeatedly.

## This function creates object that can cache inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL ## assign NULL to inverse of matrix
  
  ## change the input matrix
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }

  ## get the input matrix
  get <- function() {
    x
  }
  
  ## set the inverse of matrix
  setInv <- function(mInverse) {
    mInv <<- mInverse
  }
  
  ## try to get cached inverse of matrix (otherwise return NULL in mInv)
  getInv <- function() {
    mInv
  }
  
  ## return list of above functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of matrix object returned by make
## CacheMatrix function. If the inverse has already been computed
## (and the matrix object is unchanged), then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## try to get inverse (if previously calculated)
  mInv <- x$getInv()
  
  ## if inverse was previously calculated, return this
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  
  ## if inverse was not previously calculated, calculate and cache it
  data <- x$get() ## get the matrix
  mInv <- solve(data, ...) ## calculate inverse of the matrix x (only once)
  x$setInv(mInv) ## cache inverse of the matrix x
  
  mInv ## Return a matrix that is the inverse of 'x'
}