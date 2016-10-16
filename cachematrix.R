
?solve
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmat <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  getmat <- function(){ x }
  
  setinv<-function(inv){m<<-inv}
  getinv<-function() m
    list(setmat = setmat,
         getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    callmat<-x$getinv
  data1 <- x$getmat()
  callmat <- solve(data1, ...)
  x$setinv(callmat)
  callmat
  
          ## Return a matrix that is the inverse of 'x'
}


matt<-makeCacheMatrix(matrix(c(2,4,5,3),2,2))
# matt$getmat()
# matt$setmat(matrix(1:4,2,2))

cacheSolve(matt)

           


