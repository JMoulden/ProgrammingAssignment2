## Matrix inversion can be a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it again when needed. 

## This program contains functions to compute the inverse of 
# a matrix (assumed to be invertible), and to cache the result 
## so that it can be retrieved when needed without needing to 
## re-compute. If the matrix has changed, the cached inverse 
## matrix will be cleared.

## This program takes advantage of the lexical scoping 
## rules of the R language and illustrates how they 
## can be manipulated to preserve state inside of 
## an R object.


## makeCacheMatrix - This function creates a special "matrix" 
## object that can cache its inverse. The argument x is
## assumed to be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing inv_x
  inv_x <- NULL
  
  ## Declaring functions
  
  ## set() - Function to set matrix x with value 
  ## in argument y, and to clear cached inverse matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  ## get() - Function to return original matrix x
  get <- function() x
  
  ## setinverse() - function to set the inverse of 
  ## matrix x
  setinverse <- function(invx) inv_x <<- invx
  
  ## getinverse() - funtion to return the cached inverse 
  ## of matrix x
  getinverse <- function() inv_x
  
  ## return declared functions as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - This function computes the inverse of the 
## special "matrix" x returned by makeCacheMatrix above. 
## If the inverse or argument x has already been computed 
## (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  ## try getting cached inverse of x 
  xinv <- x$getinverse()
  
  ## if inverse of x is cached (i.e. not null), 
  ## return the cached inverse of x
  if(!is.null(xinv)) {
    message("Getting cached inverse matrix...")
    return(xinv)
  }
  
  ## call solve on x 
  xinv <- solve(x$get())
  
  ## cache the resulting inverse matrix by calling 
  ## setinverse() from function makeCacheMatrix
  x$setinverse(xinv)
  
  ## return inverse matrix
  xinv
}

