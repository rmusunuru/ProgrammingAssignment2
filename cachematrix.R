## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly and to take advantage of the scoping rules 
## of the R language and how they can be manipulated to preserve state inside of an R object


## This function creates a special "matrix" object that can cache its inverse
## makeCacheMatrix takes a matrix, saved in the private variable x

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse-matrix to NULL during the first call to makeVector
  # this is needed because ig getinverse() is called immediately after
  # the makeCacheMatrix funciton is constructed, without a call to setinverse
  # we know we must first calculate the inverse of a matrix in cacheSolve.
  m <- NULL
  # funciton to set a new value for the underlying matrix
  # this invalidates the cached inverse of a matrix, m
  # we use the <<- operator to set the value of x and m because we want
  # to modify x and m defined in the enclosing environment (created
  # when makeCacheMatrix was first called), not in the environment local to set(),
  # in which x and m are undefined.
  # we must reset m to NULL since we are modifying the underlying
  # matrix and the cached value is no longer the valid
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  # getter function for underlying matrix
  # in R the return value of a function is the last statement.
  # all of these functions could have been written as:
  # return(x), etc... as the last line.
  get <- function()
  {
    x
  }
  # set the inverse of the matrix x. Called by cacheSolve,
  # this is pretty weird style, but then so is the whole set up.
  # again we use the <<- operator because we want to modify the m defined
  # in the enclosing function makeCacheMatrix, not the m local to setinverse,
  # which would be undefined.
  setinverse <- function(solve)
  {
    m <<- solve
  }
  # returns the inverse of a matrix. Will be null if setinverse has not been called or
  # if set is called after the last call to setinverse
  getinverse <- function()
  {
    m
  }
  # return value of the makeCacheMatrix function is a list
  # of functions (and variables if we wish) that we want to expose
  # as public. these are accessed with the $ operator. Any variables
  # declared inside makeCacheMatrix but not exported as part of this list
  # are private...they are inaccessible to any caller of makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## cacheSolve takes a caching matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  # get the inverse of the matrix defined inside x.
  # we can use the $ operator to access the function since it was
  # defined in the list of function pointers returned by the call to
  # makeCacheMatrix
  m <- x$getinverse()
  # if we've already computed the inverse and stored it via setinverse(),
  # and have not invalidated the cache by calling set(), return the cached
  # version of x
  if(!is.null(m))
  {
    message("getting cached data")
    # we have to explicily use return here otherwise we'd keep
    # executing the code after the if conditional ends. Since
    # the cached version is good, just return it and we are done.
    return(m)
  }
  # either we havent computed the cached version yet, or we've called
  # set() previously and invalidated the cache.
  # call get() to get the underlying matrix
  data <- x$get()
  # calculate the inverse of the underlying matrix, passing with it
  # any varargs passed to cacheSolve
  m <- solve(data, ...)
  # now set the inverse in x so we cache it and dont need to needlessly
  # recompute it
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
        
}
