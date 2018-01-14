## MakeCacheMatrix is creates a special "matrix" object that can cache its inverse
## cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## MakeCacheMatrix - has setter and getter methods and also methods to set and return inverse matrices 

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the return matrix variable
  imtrx <- NULL
  
  ## Set or reset
  set <- function(y) {
    x <<- y
    imtrx <<- NULL
  }
  
  ## Return the input matrix
  get <- function() x
  
  ## Set the inverse on the cache
  
  setinverse <- function(invMatrix) imtrx <<- invMatrix
  
  ## Return the inverse matrix from the cache (if cache is not initialized, return null)
  getinverse <- function() imtrx
  
  ## List of functions supported
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve - Returns the cached inverse matrix, if nothing was cached, it calcs the inverse and puts it on cache
##              before returning it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  imtrx <- x$getinverse()
  
  ## Is it from Cache?
  if(!is.null(imtrx)) {
    message("getting cached data")
    return(imtrx)
  }
  
  ## Not found in Cache?
  mtrx <- x$get()
  imtrx <- solve(mtrx)
  x$setinverse(imtrx)
  imtrx
}
