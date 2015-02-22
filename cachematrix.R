## Following functions will show how lexical scoping work in R!


## This function return a list that contains function in each of its indexes.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  # cached invese matrix for future use
  getinverse <- function() m                 # return inverse matrix that cached before or null
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)       
  
}


## This function computes the inverse matrix returned by 
## makeCacheMatrix() function above. If this function called for camputing 
## the same matrix that processed before, it return cached value and don't compute again.
 

cacheSolve <- function(x, ...) {
  
# check if inverse is in cache of object x that created before with makeCacheMatrix() function
  m <- x$getinverse()

# if m is null the code below will work

  if(!is.null(m)) {                     
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
 }
