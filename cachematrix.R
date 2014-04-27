## The two functions calculate the inverse of a matrix and store
## the result in a convenient, easy-access way to facilitate retrieval,
## rather than recaluculattion if this matrix must be inverted 
## multiple times as part of a series of calculations.

## This function takes a matrix and returns four functions that get
## and set matrices and their inverses starting with the original one.

makeCacheMatrix <- function(x = matrix()) {  ## This is a function of matrices with the default an empty matrix
  m <- NULL                                  ## m is initialized as NULL
  set <- function(y) {                       ## Creates the "set" function which puts a matrix into cache
    x <<- y                                  ## and calls is "x"
    m <<- NULL                               ## m is also cached as null
  }
  get <- function() x                        ## retrieves the cached matrix
  setinverse <- function(inverse) m <<- inverse    ## sets the inverse matrix and caches as m
  getinverse <- function() m                 ## retrieves the cached inverse
  list(set = set, get = get,                 ## lists for four functions created
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function takes the vector of functions created by "makeCacheMatrix" function
## and computes the inverse

cacheSolve <- function(x, ...) {    ## The input is a vector of functions created above
  m <- x$getinverse()               ## It checks for a stored inverse
  if(!is.null(m)) {                 
    message("getting cached data")  ## If there is one then R says it's getting it
    return(m)                       ## R returns the stored inverse if there is one
  }
  
  data <- x$get()                   ## Otherwse R gets the matrix
  m <- solve(data, ...)             ## R inverts the matrix
  x$setinverse(m)                   ## Puts the inverse in cache
  m                                 ## Return a matrix that is the inverse of 'x'
}
        


