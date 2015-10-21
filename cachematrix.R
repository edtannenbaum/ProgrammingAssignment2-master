## R Programming Assignment 2

## our goal is to cache the inverse of a matrix rather than repeated calculation. 
## THE following will create and cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function which
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of inverse of the matrix
## 4. gets the value of inverse of the matrix

setwd("C:\\Users\\Eric\\Documents\\ProgrammingAssignment2-master")

## setting working directory to pull data

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

##This function returns the inverse of the matrix. 
## First, checks inverse completion. If inverse is completed...
## Than skips calculation  If not...
## Calculate inverse and sets the value in the cache via "setinverse" function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Returning program to calculate matrix of the inverse of x

## creating sample matrix

x = rbind(c(1, .33), c(-.33, 1))

## creating object for function makecacheMatrix(x)

m = makeCacheMatrix(x)

## getting function

m$get()

## getting inverse cached data

cacheSolve(m)

##done
