## Problem definition - Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly.
## Write a function for "makeCacheMatrix" which will creates a special 
## "matrix" object that can cache its inverse.

## makeCacheMatrix will :
## set and get the value of the matrix
## set and get the value of inverse of the matrix

## Assumption: the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
 invM <- NULL
 set <- function(y){
   x <<- y
   invM <<- NULL
 }
 get <- function() x
 setinversematrix <- function(inverse) invM <<- inverse
 getinversematrix <- function() invM
 list(set = set, get = get,
      setinversematrix = setinversematrix,
      getinversematrix = getinversematrix)
}


##  Below function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getinversematrix()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  matrix_data <- x$get()
  invM <- solve(matrix_data)
  x$setinversematrix(invM)
  invM
}

## Some tests:
## Create a matrix
x <- rbind(c(1,5),c(5,1))
## Create a cachematrix
cm <- makeCacheMatrix(x)
## Get the matrix being created
cm$get()
## Get the inverse matrix
cm$getinversematrix()
## 1st iteration cache - no cache
cacheSolve(cm)
## 2nd iteration cache - cache is returned
cacheSolve(cm)

