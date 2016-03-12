## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  maxinv <- NULL
  setmax <- function (y){
    x <<- y
    maxinv <<- NULL
  }
  getmax <- function() x
  setmaxinv <-function(inverse)   maxinv <<-inverse
  getmaxinv <- function() maxinv
  list(setmax=setmax,getmax=getmax,setmaxinv=setmaxinv,getmaxinv=getmaxinv)
  
  
  
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setmaxinv function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  maxinv <-x$getmaxinv()  
  if(!is.null(maxinv)){
    message("retriving cashed data")
    return(maxinv)
    
  }
  matx <-x$getmax()
  inv <-solve (matx, ...)
  x$setmaxinv(inv)
  inv
  
  
  
  
}
