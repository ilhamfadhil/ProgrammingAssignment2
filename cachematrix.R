## This function creates an R object that stores a vector and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## initilize object m 
  set <- function(y){ ## define setter
    x <<- y ## to cleared memory
    m <<- NULL ## to cleared memory
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function retreived cached object from makeCacheMatrix and 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
