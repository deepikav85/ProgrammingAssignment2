## These functions store a matrix and caches its inverse


## The first function creates special matrices to cache the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat<-NULL
  set<-function(y){
      x<<-y
      inverse_mat<<-NULL
  }
  
  get<-function() x
  setinverse<-function(inverse) inverse_mat<<-inverse
  getinverse<-function() inverse_mat
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function calculates the inverse of the matrix. It checks the caches
##first and retrieves the value if it has already been calculated, otherwise 
## calculates the inverse of the matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
        
  inverse_mat<-x$getinverse()
  if(!is.NULL(inverse_mat)){
    message("getting chached inverse matrix")
    return(inverse_mat)
  }
  mat<-x$get()
  inverse_mat<-solve(mat,...)
  x$setinverse(inverse_mat)
  inverse_mat
}
