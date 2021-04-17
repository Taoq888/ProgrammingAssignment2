## Put comments here that give an overall description of what your
## functions do

# The function CacheSolve() can get the inverted matrix of x and retrieve
# the value from the cached value that is stored in the makeCacheMatrix()
# environment, which is similar with the example in assignment 2.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL    # Assign the NULL to s object in the parent environment.
  }
  get <- function() x
  setinverseM <- function(solve) 
    {s <<- solve} 
  getinverseM <- function() s
  list(set = set, get = get,
       setinverseM = setinverseM,
       getinverseM = getinverseM)
}

cacheSolve <- function(x, ...) {
  s <- x$getinverseM()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)  
  x$setinverseM(s)  ## Return a matrix that is the inverse of 'x'
  s
}
# Test the function using two matrix M and M2;
M <- matrix(c(-1,3/2,1,-1),nrow = 2,byrow =TRUE)
M2 <- matrix(2)
x <- makeCacheMatrix(M)
cacheSolve(x)
cacheSolve(x)

rm(list=ls())


