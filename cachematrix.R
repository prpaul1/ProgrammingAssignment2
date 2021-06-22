## This script to inverse the matrix entered

## Make Cache Matrix to get and set the methods

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<-NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve the calling function 

cacheSolve <- function(x,...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("PRIYAM: getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}