
## Below are two functions that are used to create a special
## object that stores a numeric matrix and cache's its inverse. 

## This function creates a special "matrix", which is really a list containing a function to:
## 1:   set the value of the Matrix
## 2:   get the value of the Matrix
## 3:   set the value of the Inverse of matrix
## 4:   get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above. 
## If so,
## then cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
