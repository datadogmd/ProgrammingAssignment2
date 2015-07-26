## makeCacheMatrix() and cacheSolve calculate the inverse of a non-singular matrix
## and cache the result for future use.  This is useful if the the matrix does not change
## and the inverse matrix is needed more than once.

## makeCacheMatrix() creates a list that contains the function to calculate the
## inverse matrix: solve(), and a cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve() take the output list from makeCacheMatrix() as input.  If the inverse
## matrix exists it return the cached data, otherwise it calculates the inverse, and
## updates the list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
