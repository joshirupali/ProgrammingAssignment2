

##'makeCacheMatrix' function takes a matrix as an arguement which sets and gets the matrix as well as sets and gets the inverse of matrix.
##it returns a list of 4 function which in turn return the matrix when called.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmat <- function(y=matrix()) {   ##sets the value of the matrix
    x <<- y        
    m <<- NULL
  }
  getmat <- function(){ x }       ##gets the value of the matrix
  
  setinv<-function(inv){m<<-inv}      ##sets the value of the matinverse of matrix
  getinv<-function() m                ##gets the value of the matinverse of matrix
    list(setmat = setmat,
         getmat = getmat,
         setinv = setinv,
         getinv = getinv)          ##returns list
}


## 'cacheSolve' function checks if the inverse is already calculated, if not returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    callmat<-x$getinv  ##call to getinv() if inverse is present
  
      if(!is.null(callmat)) {       ##checks if inverse is not null and returns the available inverse. 
      message("getting cached data")
      return(callmat)
    }
    
  data1 <- x$getmat()            ##gets the matrix which was passed in 'makeCacheMatrix'
  callmat <- solve(data1, ...)        ##calcutes the inverse using solve function
  x$setinv(callmat)     ##sets the new inverse of matrix
  callmat  ## Return a matrix that is the inverse of 'x'
  
          
}

## function call
matt<-makeCacheMatrix(matrix(c(2,4,5,3),2,2))

cacheSolve(matt)

           


