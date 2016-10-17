## these two functions will create a matrix, calculate inverse of the matrix
## and retrive the result

## create a matrix based on input parameter, initialize its inverse,
## and retrive the matrix and the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## check if the inverse has been calculated
## if the inverse has been calculated, return the result
## otherwise, calculate the inverse and store it use the setinverse function

cacheSolve <- function(x, ...) {
       inv<-x$getinverse()
       if(!is.null(inv)){
         message("getting cached data")
         return (inv)
       }
       data<-x$get()
       inv<-solve(data)
       x$setinverse(inv)
       inv
}
