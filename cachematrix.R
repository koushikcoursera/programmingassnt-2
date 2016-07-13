# Matrix manipulation functions . With various options by koushik Gangavaram
#
#
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
## makeCacheMatrix creates a list function -  F U N C T I O N - 1
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  }


##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
## This function is for  matrix invertible, if not will change it   F U N C T I O N - 2
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
