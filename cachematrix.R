## This work is to write a pair of functions that cache the inverse of a matrix
## functions do

## This function creates a special "Matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinversem<-function(inversem) m<<-inversem
  getinversem<-function() m
  list(set=set,get=get,setinversem=setinversem,getinversem=getinversem)

}


## This function calculates the inverse of the special "Matrix" created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinversem()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinversem(m)
  m
}
