## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

####################################################################
##m=makeCacheMatrix(x) This function creates a list of functions
##which it assigns to m. The list contains a function to set the value of 
##the matrix, get the value of the matrix, set the inverse of the matrix
##get the inverse of the matrix

####################################################################
####################################################################
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


####################################################################
## Write a short comment describing this function
##
####################################################################
####################################################################
#cacheSolve() looks through the cache to see if an inverse matrix has already 
#been computed and exists in the cache for a square
#invertible matrix. If the value exists it retrieves it, 
#if not it computes the new value
#
####################################################################

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setmatrix(m)
  m
}







