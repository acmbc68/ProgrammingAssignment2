
## Answer file to Coursersa - R Programming - 2nd programming assignment
## This file contains two functions
## cacheSolve(x) - Returns inverse of square matrix passed as argument X.  Caches inverse matrix, and returns cached value if inverse matrix has already been calculated
## makeCacheMatrix(x) - caches (stores) the value of the inverse matrix for x
## Both functions work together, with makeCacheMatrix being called from cacheSolve


## cachematrix.R
##
## Create a cached matrix object that to solve 
## the inverse of a matrix, only calculating the inverse the first time, 
## and using cached data from then on
##
## how to use - 
##  aMatrix <- matrix(c(1, 2, 2, 1), nrow=2, ncol=2)
##  cachedMatrix <- makeCacheMatrix(aMatrix)
##  cacheSolve(cachedMatrix)
##
##  cachedMatrix$set(M)      # Change the matrix being cached.
##  aMatrix <- cachedMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Function (Private scope) which contains the cached inverse matrix of x
##  cacheMatrix$getInverse()                 # Function (Private scope) to get and return the cached inverse of x

## Create a cached Matrix object for an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() x ## Returns the value passed
  
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse ## Returns the cachedinverse matrix.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of an Matrix object, calculating the inverse if it is not cached, and returning the cached value if it is already stored

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("returning cached data for inverse matrix")
    return(inverseMatrix)
  }
  message("no cached data. Calculating and returning inverse matrix")
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
