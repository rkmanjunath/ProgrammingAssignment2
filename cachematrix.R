## The script contains a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## Parameters: x - a square invertible matrix

## Returns: a list containing a function to 
## set() - set the value of the matrix, x
## get() - get the value of the matrix, x
## steinv() - set the value of the inverse, x_inv
## getinv() - get the value of the inverse, x_inv

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) x_inv <<- inv
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() above using solve() function. If the inverse 
## has already been calculated (and the matrix has not changed), then the 
## cacheSolve() retrieves the inverse from the cache.

## Parameters: x - the output of the function makeCacheMatrix which is a list 

## Returns: x_inv - the inverse of the matrix 'x'

cacheSolve <- function(x, ...) {
        
        x_inv <- x$getinv()
        
        if(!is.null(x_inv)){
                message("getting cached data")
                return(x_inv)
        }
        
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv                       
}

