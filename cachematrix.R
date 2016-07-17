## makeCacheMatrix creates a cache-able matrix from some given x
## cacheSolve returns the inverse of some matrix, returning the cache'd data
## when such data exists.

## makeCacheMatrix creates a cache-able matrix from some given x


makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Cachesolve returns the inverse of a matrix or its cache'd inverted matrix

        
cachesolve <-function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m ## Return a matrix that is the inverse of 'x'
}



