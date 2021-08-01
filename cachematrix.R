## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  iv <- NULL
  set <- function(y) { 
    x <<- y
    iv <<- NULL
  }
  get <- function() {x} 
  setInver <- function(inverse) {iv <<- inverse}
  getInver <- function() {iv}
  list(set = set, get = get, setInver = setInver, getInver = getInver)
}



cacheSolve <- function(x, ...) {
  iv <- x$getInver()
  if(!is.null(iv)) { 
    message("getting cached data")
    return(iv) 
  }
  oc <- x$get()
  iv <- solve(oc, ...)
  x$setInver(iv)
  iv
}