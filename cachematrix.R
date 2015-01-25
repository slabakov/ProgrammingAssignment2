## makeCacheMatrix defines 4 functions
##    setm: assigns the input matrix to variable "x" in the parent context
##    getm: reads that matrix
##    setinv: store the inverse of the matrix in "inverse" (parent environment)
##    getinv: read the value of "inverse"

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      setm <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      getm <- function() x
      setinv <- function(inv) inverse <<- inv
      getinv <- function() inverse
      list(setm = setm, getm = getm,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve takes a matrix argument "x"
## and calculates an inverse matrix for it
## unless it finds "inverse" already non-null (cached)
## in which case it returns the cached value of "inverse"

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinv()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$getm()
      inverse <- solve(data)
      x$setinv(inverse)
      inverse
}
