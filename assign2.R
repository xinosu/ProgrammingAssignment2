 ## x is a square invertible matrix
  ## This function returns a list functions to
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  ## This list of function is the parameter passed to cacheSolve()
  ## This function is used to store/get matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
 
  r <- matrix()
  
  ## A new matrix y can be stored in a matrix that is not in current environment
  set <- function(y) {
    x <<- y
    r <<- matrix ()
  }
  
  ## get matrix x
  get <- function() x
  
  
  ## Just store the inverse and store it in r, not cauculate the reverse
  setinverse <- function(inverse) r <<- inverse
  
  ## get reverse
  getinverse <- function() r
  
  ## return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function only return the inverse and store it in cache if necessary
  ## x is the list of function returned from makeCacheMatrix(), it isdifferent from the parameter matrix x in makeCacheMatrix()
 cacheSolve <- function(x, ...) {
  
  r <- x$getinverse()
  
  ## if the inverse has already been calculated
  if(!is.null(r)) {
    ## return the reverse from cache and exit this funtion
    message("getting cached data")
    return(r)
  }
  
  ## if not, calculate the inverse
  data <- x$get()
  r <- solve(data, ...)
  
  ## store the reverse in cache
  x$setinverse(r)
  r
}