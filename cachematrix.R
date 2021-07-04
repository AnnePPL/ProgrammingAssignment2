## 
##
##set x as matrix
##set value of z as null

## set matrix and inverse of matrix

makeCacheMatrix <- function(x = matrix(sample(1:50,5),2,2)) {
  z<-null
  set<-function(matrix){
    x<<-matrix
    z<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(Inverse){
    z<<-Inverse
  }
  getInverse<-function(){
    z
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  x<-x$getInverse()
  if(!is.null(x)){
    message("get inverse matrix")
    return(x)
  }
  data<-x$get()
  x<-solve(data)%*% data
  x$setInverse(x)
  x
  
}


