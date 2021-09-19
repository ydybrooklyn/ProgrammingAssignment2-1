## caching the inverse of a matrix rather than compute it if it has
## been already calculated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL 
  set <- function(y) { 
    x <<- y 
    n <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) n <<- inverse 
  getinverse <- function() n 
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse) 

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- mean(data, ...)
  x$setinverse(n)
  n
        ## Return a matrix that is the inverse of 'x'
}
