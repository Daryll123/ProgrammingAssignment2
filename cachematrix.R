## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ip <- NULL
  
  set <- function(matrix) { 
    y <<- matrix
    ip <<- NULL
  }
  
  get <- function() {y} 
  
  setIM <- function(inverse) {ip <<- inverse}
  getIM <- function() {ip}
  list(set = set, get = get, setIM = setIM, getIM = getIM)
}



cacheSolve <- function(x, ...) {
  
  y <- x$getIM()
  
  if(!is.null(y)) { 
    message("getting cached data")
    return(y) 
  }
  
  
  od <- x$get()
  
  y <- solve(od) %% od
  
  x$setIM(y)
  y
}