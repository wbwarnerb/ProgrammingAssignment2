## These functions calculate the inverse of a matrix.  They also add the ability
## to grab a cached inverse calculation, so it doesn't have to be computed again
## each time.  

## The makeCacheMatrix function sets the value of the matrix, gets the value of 
## matrix, sets the value of the inverse, gets the value of the inverse. It also
## uses m as a counter of sorts. The value of m is set to NULL initially.  But when a 
## matrix inversion calcuation is done, we change the value of m to be the 
## inverse (no longer NULL.)  This tells the solve function that it's in cache
## and doesn't need to be recalcuated from scratch.
## What is returned is a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function(){m}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function, first sets m to the x$getinverse().  Then if m returns 
## as NULL, we do the inversion calcuation for the first time on the matrix
## being passed in.  We set the variable 'data' to x getting passed in, then
## we set the variable m to solving(data) - which gives us the inverted matrix.
## We use the setinverse(m) call to take the inverted matrix and pass
## it back to the makeCacheMatrix function so it's stored.
## If m has had the matrix inverted before ( we check this using
## the if logic), we validate this (if m is not NULL), then we grab the result 
## in cache of the previous calculation.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
