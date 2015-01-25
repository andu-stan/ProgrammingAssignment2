## Create a special "matrix" objects using makeCacheMatrix
## function and calculate the inverse of the matrix, or 
## retrive the inverse from cache if it has already been 
## calculated, using the function cacheSolve

## This function creates a special "matrix" object that can cache 
## its inverse. 

makeCacheMatrix <- function(x = numeric()) {
  ## Verify if matrix is square
  if(sqrt(length(x))%%1!=0) {
    print('ERROR - Matrix you want to generate is not square.')
    return()
  }  
  ## Create special "matrix"
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() matrix(x, nrow=sqrt(length(x)), ncol=sqrt(length(x)))
  setinv <- function(solve) a <<- solve
  getinv <- function() a
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
