## Assignment 2 description: Write a pair of functions that cache the inverse of a matrix.
## Objective: Understand lexical scoping in R
## makeCacheMatrix will create a special matrix object; cachesolve will check to see if the inverse
## of the makeCacheMatix already exists and return it instead of creating the inverse.

## The makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse_x <- NULL
  set <- function(y) {
      x <<- y
      inverse_x <<- NULL    
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}
  
## The CacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, and the matrix has not changed, 
## then cacheSolve will retrieve the inverse from the cache. If it has not been calculated,
## then the cacheSolve will use 'solve' to compute it, then set inverse_x, and return inverse_x
## as the cached inverse matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x.
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("getting cached data")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$setinverse(inverse_x)
  inverse_x
}
