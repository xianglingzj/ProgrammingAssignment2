## Put comments here that give an overall description of what your
## functions do
# Description
#     This is a cache-enabled implemetnation of the matrix inverse function
#
#     This file contains two parts: 
#     Part 1: makeCacheMatrix(), an encapsulated matrix function with an
#   embedded inverse matrix in the global environment and related member
#   functions to set(write)/get(read) the matrix itself or the inverse matrix,
#   and
#     Part 2: cacheSolve(), a function which calculate the inverse of the 
#   matrix, which will consult the embedded inverse variable if possible.
#

## Write a short comment describing this function
# Descritpion
#
# makeCacheMatrix creates a matrix with an embedded inverse matrix
#
# Usage
# 
# makeCacheMatrix( x = matrix() )
#
# Arguments
#
# x     an optional numeric matrix
#
# Details
#
# This function returns a list of functions on a given matrix, which are:
#   set(y):          Set the value of matrix to a numeric matrix y.
#   get():           Get the value of the numeric matrix
#   setinv(inverse): Set the value of the embedded inverse matrix to a numeric
#                    matrix inverse
#   getinv():        Get the value of the embedded inverse numeric matrix
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setinv <- function(inverse) {
    inv <<- inverse
  }
  getinv <- function(){
    inv
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
# Descritpion
#
#   cacheSolve calculate the inverse of the matrix in the makeCacheMatrix
# function with reference to the embedded inverse value if possible
#
# Usage
# 
# makeCacheMatrix( x, ... )
#
# Arguments
#
# x     an makeCacheMatrix object with an embedded numeric inverse matrix
# ...   additional arguments to be passed to a normal solve() fucntion of
#       a numeric matrix
#
# Details
#
#   This function try to get the embedded inverse matrix of the makeCacheMatrix.
# The function will return the embedded inverse matrix if it exists; and if the
# embedded inverse matrix does not exist, it calculates the inverse of the 
# numeric matrix and return the inverse after storing it to the embedded inverse
# matrix.
cacheSolve <- function(x, ...) {
    inv <-x$getinv();
    if(!is.null(inv)){
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
        ## Return a matrix that is the inverse of 'x'
}
