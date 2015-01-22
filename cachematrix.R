## The functions below exemplifies how caching can be used to improve
## performance of R programs. Caching is to store data that results from 
## intensive computation that can be recovered by other parts in the program.

## This function stores the matrix (variable x) and its inverse (variable 
## inverse). It behaves like a class from the object oriented programming
## paradigm (x and inverse are attributes and the functions get, set,
## setinverse and getinverse are the methods). Actually, this "class" is 
## implemented through the list data structure.
## To create a new object you can use 
##    m = makeCacheMatrix() or  m = makeCacheMatrix(matrix(1:4,2,2))
## Using the methods
##    m$get()  or  m$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the argument x. Please note 
## it assumes that x is an object created by makeCacheMatrix. The function 
## cacheSolve checks if the inverse was previously calculated and returns it, 
## otherwise the inverse is computed and the function setinverse stores the 
## result.
##
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  return(i)
}