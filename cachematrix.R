# Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
# 
# The first function, `makeCacheMatrix` creates a special "Matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the Matrix
# 2.  get the value of the Matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

## creates a special "Matrix" as described above

makeCacheMatrix <- function(x = matrix()) {
  ## intialize inverse
  m <- NULL
  ## allow setting a value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## allow get value
  get <- function() x
  ## allow set inverse
  setinverse <- function(inverse) m <<- inverse
  ## allow get inverse
  getinverse <- function() m
  ## return list of functions as descibed in 1., 2., 3., and 4. above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates the inverse of the special "Matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check if inverse already calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    ## yes has been caluclated so can print message and return that value
    message("getting cached data")
    return(m)
  }
  ## NO, calculation not yet done so calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  ## ... and set for future use i.e. CACHE it
  x$setinverse(m)
  ## now return inverse
  m
}
