## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is storing a list of function and apply these function on input matrix.
#  Set function changes the matrix stored in main function and reseting m so that new value can be used.
#  get function have new matrix for calculating new inverse.
#  setinverse fuction is setting the vaule whatever assigned to it.It is not inverse of the matrix
#  getinverse fuction is just to get what is set in setinverse.
#  We are creating list of functions so that they can be used when main function "makeCacheMatrix" is assigned to an object.
## Write a short comment describing this function
## makeCacheMatrix is storing a list of function and apply these function on input matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  # Reset variable m
  m <- NULL
    
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    
  get <- function() { x }
    
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
    
}


## cacheSolve function verify value of m stored by getinverse if it is , it will display inverse. If not,
## compute inverse of the new matrix updated by makeCacheMatrix and return the result. 
   

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
   ## This part is working as else of above "if statement". It means if the is nothing in memory at m,
   ## take new value stored by get() in main function, compute inverse and store new value in m and call setinverse 
   ## function to set new inverse.

   data <- x$get()
   m <- solve(data,...)
   x$setinverse(m)
   m
}
