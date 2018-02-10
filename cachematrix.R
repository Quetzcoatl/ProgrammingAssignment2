## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvMat <- function(invMat) i <<- invMat
  getinvMat <- function() i
  list(set = set,
       get = get,
       setinvMat = setinvMat,
       getinvMat = getinvMat)
}

cacheSolve <- function(x, ...) {
  i <- x$getinvMat()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvMat(i)
  i
}