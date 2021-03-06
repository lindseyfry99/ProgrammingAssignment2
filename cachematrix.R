## These functions will cache the inverse of a matrix

## This function will create a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  getInverse <- function()inverse
  setInverse <- function(solveMatrix)
  inverse <<- solveMatrix
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function will compute the inverse of the matrix returned by the function above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
