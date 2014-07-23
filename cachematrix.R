## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## First we create a variable to save the inverse in.
    inverse <- NULL
    ## Create the functions to set, get, set inv and get inv
    set <- function(y) {
        x <<- y
        ## Making sure to nullify inverse in case the matrix changes.
        inverse <<- NULL
    }
    get <- function() x
    ## Settign the inv is done in the higher environment hence the <<-
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    ##Return a "Special" matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if we have the inverse matrix saved and if so return it
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ##The solve function gives the inverse of a matrix
    inv <- solve(data, ...)
    ## save the inv
    x$setinv(inv)
    ## return the inv
    inv
}
