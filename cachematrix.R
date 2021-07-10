## "makeCacheMatrix" function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL               
  ## I define the "setter" for the matrix
  set <- function(y) {    
    ## I  initialize the two objects, "x" and "i"
    x <<- y
    i <<- NULL
  }
  ## I define the "getter" for the matrix
  get <- function() x
  ## I define the "setter" for the inverse
  setInverse <- function(solve) i <<- solve
  ## I define the "getter" for the inverse
  getInverse <- function() i
  ## I give the name "set" to the "set" function
  list(set = set,                 
       ## I give the name "get" to the "get" function
       get = get,
       ## I give the name "setInverse" to the "setInverse" function
       setInverse = setInverse,        
       ## I give the name "getInverse" to the "getInverse" function
       getInverse = getInverse)        
}


## "cacheSolve" function

cacheSolve <- function(x, ...) {
  ##The "cacheSolve" function calls the "getInverse" function on the input object
  i <- x$getInverse() 
  ##It checks to see whether the result is NULL 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##If the result of !is.null(i) is FALSE, "cacheSolve" gets the matrix from the
  ## input object, calculates the inverse, uses the  "setInverse" function on the
  ## input object to set the inverse matrix on the input object and then returns
  ## the inverse matrix by printing "i"
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}