## 11.20.15 pattonj6
## makeCacheMatrix uses Cachingmean.R/makeVector as the base code
## This first function creates a special "matrix" object that
## can cache its inverse.

## There are 4 functions inside makeCacheMatrix function: set, get, 
## setInverse, and getInverse.

## set changes the matrix stored in the main function.
## get returns the matrix stored in the main function.
## setInverse changes the inverse value "i" and stores it in main
## function.
## getInverse returns the inverse value "i" stored in the main
## function.
## The list at end stores the 4 functions inside the main 
## makeCacheMatrix function.   

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This second function returns the inverse of the 
## special "matrix" cached in makeCacheMatrix.

## Input into this function is the object where 
## makeCacheMatrix is stored. 
## eg. a <- makeCacheMatrix(matrix())
## eg. cacheSolve(a)

## If makeCacheMatrix passes a value(not NULL) to cacheSolve
## then cacheSolve just returns that value.
## If makeCacheMatrix passes a NULL value, then it calculates
## the inverse using the solve function and assigns to the 
## x$setInverse(m) object from makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

