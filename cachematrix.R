## makeCacheMatrix define a matrix with a special list of functions 
## to assign and obtain matrix properties
## cacheSolve calculates inverse of the matrix if it is not already cached

## make CacheMatrix defines a list of functions that enables calculation...
## and assignment of basic matrix properties
## the functions 'set': assigns values to the matrix
## the function 'get':  obtains the value of the matrix
## the function 'setInv': caches the inverse of the matrix once available
## the function 'getInv': obtains the cached value of the matrix if available

makeCacheMatrix <- function(x = matrix()) {
    mI <- NULL # Initialize matrix inverse mI to NULL
    
    # Function to 'set' the value of the matrix with passed value
    set <- function(y) {
      x <<- y # <<- assigns makeCacheMatrix x to the passed value y
      mI <<- NULL # Matrix inverse is assinged to NULL
  }
  get <- function() x # Function to 'get' the values of the matrix
  setInv <- function(inv) mI <<- inv # Assigns mI from outside the function to the inverse
  getInv <- function() m # Function to 'get' the inverse value of the matrix
  # Final list returned
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve first obtains the matrix inverse property
## if the inverse is available (not NULL) then returns it
## if the inverse is not available it calculates it, assigns the value, then returns it

cacheSolve <- function(x, ...) {
    # Obtain the inv value of current matrix
    inv <- x$getInv(x)
    # Check if value is available (not NULL) and obtain the cached value
    if(!is.null(inv)){
      message("Getting cached data")
      return(inv) # Returns vached value
    }
    # If value is not available (is NULL), calculate from matrix data using solve()
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv) # Assign matrix propery inverse to calculated value for cache
    inv # Returns inverse value
}
