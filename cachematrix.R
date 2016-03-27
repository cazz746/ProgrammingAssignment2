## used in future calculations to increase the speed.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

makeCachematrix <- function(x = matrix()) {
  inv<-NULL
  set<- function (y) {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setmatrix<-function(solve) inv<<-solve
  getmatrix<-function()inv
  list (set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}



##Cachesolve is the function to compute the inverse of the special "matrix" returned 
##by makecachematrix.It first checks to see if the inverse has already been saved un a cache.
##If it hasn't it will calculate it.

cacheSolve <- function(x, ...) {
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setmatrix(inv)
  inv
}