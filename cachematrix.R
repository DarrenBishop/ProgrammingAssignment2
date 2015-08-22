## makeCacheMatrix and cacheSolve are a pair of functions that can
## be used together to store an invertible matrix along with its
## calculated inverse.
##
## Example:
## 
##     m <- matrix(c(rnorm(3), runif(3), rnorm(3, 1)), 3, 3)
##     m
##     cm <- makeCacheMatrix(m)
##     cm$get()
##     cacheSolve(cm)
##     im <- cacheSolve(cm)
##     im
##     icm <- makeCacheMatrix(im)
##     icm$get()
##     cacheSolve(icm)
##     cacheSolve(icm)


## Makes a cache-matrix encapsulating an invertible matrix and its
## inverse and providing accessor functions to both i.e.
##
##  set
##  get
##  setinverse
##  getinverse
##
## Using the $set accessor will clear the cached inverse, thus the
## inverse will need to be NULL-checked; it is recommended to
## access the inverse with the companion cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## Retrieves the inverse matrix of the provided cache-matrix,
## calculating and caching the inverse if it is not present.
##
## The solve function is used to calculate the matrix' inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  imt <- x$getinverse()
  
  if (!is.null(imt)) {
    
    message("Getting cached inverse")
    return(imt)
  }
  
  mt <- x$get()
  imt <- solve(mt, ...)
  x$setinverse(imt)
  imt
}
