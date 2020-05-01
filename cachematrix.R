## Put comments here that give an overall description of what your
## functions do

## These functions are used to cache the inverse of a matrix so that it does not need to be calculated repeatedly, saving on computing power and time
## This happens by creating a custom 'matrix' object which actually is a list of functions to get or set the values of the matrix and its inverse
## The inverse is found by using the cacheSolve function

## Write a short comment describing this function
## This function creates a special custom 'matrix' object that is actually a list of functions to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL     ## The inverse matrix of x, initialised to NULL
    set <- function(y)  ## this allows us to set the value of the matrix
    {
        x <<- y     ## assign the new matrix y to x
        inv <<- NULL    ## reset the inverse of this matrix to NULL
    }
    
    get <- function() x     ## simple function to get the value of the matrix; returns x
    setinverse <- function(inverse) inv <<- inverse     ## function to set the inverse of this matrix. Simply assigns inv to be the value passed in
    getinverse <- function() inv    ## function to get the inverse; returns inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    ## return a list of the functions
}


## Write a short comment describing this function
## This function takes the custom 'matrix' object, and returns its inverse. If the inverse has been calculated before, it returns the cached value
## otherwise it calculates the inverse and caches it
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()   ## get the cached value
    if(!is.null(inv))   ## if the cached value isn't NULL, i.e. we have calculated it before
    {
        return(inv) ## return the cached value
    }
    
    ## if we are here, the cached value of inverse is null
    mat <- x$get()  ## get the matrix
    inv <- solve(mat)   ## find its inverse
    x$setinverse(inv)   ## cache the inverse
    inv     ## Return a matrix that is the inverse of 'x'
}
