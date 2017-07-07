## Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly. The functions in this file have been created to cache the inverse 
## of an invertible matrix to avoid the computation repeatedly.

## The function "makeCacheMatrix" is a function that creates a list of the
##following four function
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix<-NULL 
  set<-function(y){
    x<<-y
    InvMatrix<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) InvMatrix<<-inverse
  getinverse<-function()InvMatrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function "cacheSolve" is a function which takes an invertible matrix
## as an input and returns the inverse of that matrix.If the inverse of a 
## matrix has already been computed and the matrix has not changed then
## the function caches the inverse of the matrix from the workspace.

cacheSolve <- function(x, ...) {
        InvMatrix<-x$getinverse()
        if(!is.null(InvMatrix)){
          message("getting cached data")
          return(InvMatrix)
        }
        data<-x$get()
        InvMatrix<-solve(data,...)
        x$setinverse(InvMatrix)
        InvMatrix
        }
