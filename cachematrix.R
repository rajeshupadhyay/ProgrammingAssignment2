## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the value of the Matrix Inverse to NULL
  matinv <- NULL
  ## declare function set where the value will be cached
  ## Matrix is created for the first time.
  
  set <- function(y = matrix()) {
    x <<- y
    ## Change the value of the inverse of the matrix in case the matrix was changed
    matinv <<- NULL
  }
  
  ## Get the value of the inverse
  get <- function() x
  
  ## Calculates the inverse of matrix via built-in function solve
  setinverse <- function(solve) matinv <<- solve
  
  ## Gets the inverse
  getinverse <- function() matinv
  
  ## 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##Get the Cache of the matrix
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinverse()
  
  ## If the cache contains inverse of matrix, get the value from Cache
  if(!is.null(matinv)) {
    message("Getting cached data - Inverse of the matrix")
    return(matinv)
  }
  
  ## If cache do not contain the inverse, 
  ## then, calculate the value and then return it
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinverse(matinv)
  matinv
}
