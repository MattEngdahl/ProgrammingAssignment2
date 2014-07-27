## Functions that cache and retrieve a matrix and its inverse
## using the solve function.


## Creates a list of functions that store and retrieve data 
## from external environments,
makeCacheMatrix <- function(x = matrix()) {
    ## Clears 'm' (sets m to NULL)
    m <- NULL
    
    ## 'set' stores a matrix in x in an environment outside
    ## of this function; it clears (sets to NULL) m, also 
    ## held in an environment outside of this function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Auto-prints the value of x
    get <- function() x
    
    ## sets m as the inverse of the original matrix
    setSolve <- function(solve) m <<- solve
    
    ## Returns m (the inverse of the original matrix)
    getSolve <- function() m
    
    ## Returns the list of fuctions when running 
    ## makeCacheMatrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## cacheSolve retrieves cached inverted matrices if already
## inverted, or, if not available, inverts the matrix and 
## outputs the results of either eventuality.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Calls 'getsolve' for argument 'x' and stores in 'm'
    m <- x$getSolve()
    
    ## if 'm' is not empty, then return 'm' - the cached
    ## inversed value - and exit the function by returning
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, continue, and set data by calling the 'get'
    ## function on the argument 'x'.
    data <- x$get()
    
    ## Then store the inverted data in 'm'...
    m <- solve(data, ...)
    
    ## ...and cache the inverted data.
    x$setSolve(m)
    
    ## return 'm', now inverted.
    m
}