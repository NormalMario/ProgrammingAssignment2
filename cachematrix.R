rm(list = ls())
## Cashing the inverse of a matrix

## Function computes a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Function computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if (!is.null(s)){
        message("getting cached data.")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## Does it work?
mdat <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1", "row2"), c("col1", "col2")))
mdat

myMatrix <- makeCacheMatrix(mdat)
cacheSolve(myMatrix)
cacheSolve(myMatrix)
