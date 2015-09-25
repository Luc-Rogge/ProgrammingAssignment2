# makeCacheMatrix() creates a special "matrix" object that can cache its inverse
# initializes helpers
# $set: to set the matrix
# $get: to get the matrix
# $setinv: to cache the inverse of the matrix
# $getinv: to get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve() returns the inverse of a special matrix created thru makeCacheMatrix()
# get the cached inverse if any 
# else compute the inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
