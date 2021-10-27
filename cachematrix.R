## To cache the inverse of a matrix
## functions inverse the matrix

## Creating a cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(t){
    x<<-t
    inv<<- NULL
  }
  get<- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set= set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## Making a function to cache solve and inverse it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$ getInverse()
  if(!is.null(inv)){
    message(" Extracting catched data")
    return(inv)
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}
