## This creates a list of  functions to get/set the matirix

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

##This function checks for a chached Matric and if it is not cached calculates
## the inverrse and displays it
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