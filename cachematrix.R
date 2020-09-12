## makeCacheMatrix

## This function gets the inverse of a matrix
#makeCacheMatrix
makeCacheMatrix<-function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() {x}
  setinverse<-function(inverse){inv <<- inverse}
  getinverse<-function(){inv}
    list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
## cachesolve This function returns inverse of the matrix
cachesolve <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mdata<-x$get()
  inv <- solve(mdata, ...)
  x$setinverse(inv)
  inv
}
