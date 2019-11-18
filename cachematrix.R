## Put comments here that give an overall description of what your
## functions do

## The first function, `makeVector` creates a special "vector", which is
## really a list containing a function to

## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean

##please note that I have used inv fucntion from matlib package in my program

library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  
  #solvedinv is an input parameter passed in setinverse function
  setinverse <- function(solvedinv) invers <<- solvedinv
  getinverse <- function() invers
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invers <- x$getinvers()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  ##inv is a function that returns inverse of a matrix, from matlib package
  invers <- inv(data, ...)
  x$setinvers(invers)
  invers
}
