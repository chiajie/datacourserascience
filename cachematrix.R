## The following functions allow the creation of a matrix that contains a cache which
## is able to store the value of the inverse of the matrix once calculated. If the
## inverse of the matrix is required, the cache will first be checked if it already
## contains this value so time-consuming computation can be avoided.

## makeCacheMatrix creates a matrix that is able to store its inverse in a cache once
## that value is calculated

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a function that computes the inverse of matrices created by makeCacheMatrix.
## Before calculating the value, cacheSolve first looks into the cache to see if the
## inverse has already been calculated and stored.

cacheSolve <- function(x, ...) {
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
