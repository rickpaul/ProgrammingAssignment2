## This is a family of functions that will solve for and cache an inverted matrix.
## The goal of the function is to save repeated expensive computations.

## This function returns a matrix and related functions that can cache its 
## result (an inverted matrix).

makeCacheMatrix <- function(x = matrix()) {
    Inverted <- NULL
    set <- function(y) {
        x <<- y
        Inverted <<- solve(x)
    }
    #returns the original matrix
    get <- function() x
    getInverted <- function() Inverted
    solveInverted <- function() {Inverted <<- solve(x)}
    list(set = set, get = get,
         getInverted = getInverted,
         solveInverted = solveInverted)
}


## This function returns an inverted matrix. It first checks to see if the matrix
## is cached. If not, it computes it. If so, it computes the matrix and replaces
## the matrix in the cache.

cacheSolve <- function(x, ...) {
    #check if the caller is passing in a new matrix. 
    if(!missing(...))
    {
        # I'm a litle mad I couldn't put this in the upper (outer)
        # if statement with an '&' between them. It insisted on checking
        # both arguments.
        if(identical(x$get(),...))
        {
            message("New matrix isn't different. Getting cached data")
            return(x$getInverted())
        }
        message("Setting new matrix")
        x$set(...)
        return(x$getInverted())
    }
    else if(is.null(x$getInverted()))
    {
        message("Solving matrix inversion for first time.")
        x$solveInverted()
        return(x$getInverted())       
    }
    else
    {
        message("No matrix passed. Getting cached data")
        return(x$getInverted())
    }    
}
