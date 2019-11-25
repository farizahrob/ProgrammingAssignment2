##pair of functions that cache the inverse of a matrix

## creates special matrix and sets and gets value of matrix and sets and gets value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solvematrix) inverse <<- solvematrix
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calculates inverse of special matrix from above function
##if inverse already calculated, then gets the inverse from cache
##otherwise calculates inverse and sets value of inverse in the cache

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
