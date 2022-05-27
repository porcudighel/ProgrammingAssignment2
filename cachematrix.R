## The functions in this script calculate the inverse of a matrix, caching the result
## If the matrix was previously calculated the inverse is not recalculated, but retrieved from the cache

## The function makeCacheMatrix create a list of functions associated with a matrix input
## set() initialises the passed matrix and the resulting inverse within the parent function environment (by use of the superassignment operator <<-)
## get() returns the passed matrix
## setinv() stores a calculated inverse matrix
## getinv() returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversematrix) inv <<- inversematrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## cacheSolve returns the inverse matrix
## checks the value stored in the makeCacheMatrix object: 
## -if not null, it get the one stored together with a message and ends there
## -if null, gets the data, calculates the inverse, stores it in the object, returns the inverse

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

# WORKING EXAMPLE
# > source("cachematrix.R")
# A <- matrix(c(1,2,2,1),2,2)
# > mA <- makeCacheMatrix(A)
# > cacheSolve(mA)
# [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# > cacheSolve(mA)
# getting cached data
# [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333