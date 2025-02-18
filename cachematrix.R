
## makeCacheMatrix makes a matrix object that stores a matrix, as well as 
## methods for getting the matrix, setting the matrix, getting the inverse, and 
## setting the inverse. It caches the inverse for faster retrieval. It is 
## implemented as a list. cacheSolve solves the matrix's inverse if it hasn't 
## already been cached. If it has already been cached, then it just retrieves 
## it from the cache.

## Makes a special matrix object described above

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Solves a matrix object described above by computing it's inverse and caches
## it. If the matrix object already has an inverse cached, it just retrieves it 
## from the cache. 

cacheSolve <- function(x = list(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(x$get(), ...)
  x$setinverse(i)
  i
}
