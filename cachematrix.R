## The two functions below calculate the inverse of a matrix and caches both values. 
## In case the inverse has to be calculated again this calculation can be skipped by 
## getting the inverse from the cache which makes the program faster.

## The function makeCacheMatrix calculates the inverse of a matrix and cashes
## both.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  } 
  get<-function () x
  setinverse <-function(solve) m<<-solve
  getinverse <-function() m
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates the inverse of a matrix using the R-function solve(). 
## It gets the inverse from the cache in case the inverse of this matrix has been
## calculated before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
