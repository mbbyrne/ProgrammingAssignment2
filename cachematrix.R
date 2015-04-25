## The combination of the two functions makeCacheMatrix and cacheSolve are
## used to return the inverse of a matrix. Once makeCacheMatrix has been
## called for a matrix, cacheSolve can then be called to compute the inverse of
## that matrix. The computed inverse matrix will be cached so that subsequent
## calls of cacheSolve will return the cached value rather than computing it
## every time.



## makeCacheMatrix - this function creates a list of four functions which: 
## set the value of the matrix
## get the value of the matrix
## sets the value of the inverse of the matrix
## get the value of the inverse of the matrix
##
## Argument x: a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
       
}

## cacheSolve - function to compute and return the inverse of the matrix passed
## into the makeCacheMatrix function using the list of functions returned
## by the makeCacheMatrix function. 
## If the inverse has already been calculated and the matrix has not changed
## then the function will retrieve the cached value and show the message
## "getting cached data"
##
## Argument x: list returned by makeCacheMatrix 
## Argument ...: function accepts variable number of additional arguments
## which can be passed to the solve function

cacheSolve <-function(x, ...) {
            
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
