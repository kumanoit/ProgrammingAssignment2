## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will create a vector of special type that will hold a matrix 
## and its associated operations
makeCacheMatrix <- function(x = matrix()) {

    ## inverseMatrix will hold the inverse of a matrix
    ## it is initialised to a NULL value
    inverseMatrix <- NULL
    
    ## this function sets the value of x.
    ## Although this is not used here but it can be used later to modify matrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## this get function will return the matrix value
    get <- function() x
    
    ## setinverse function will cache the inverse of matrix
    setinverse <- function(inverse) inverseMatrix <<- inverse
    
    ##getinverse function will return the inverse of the matrix
    getinverse <- function() inverseMatrix
    
    list(get = get,
         set = set,
         getinverse = getinverse,
         setinverse = setinverse)
}


## Write a short comment describing this function

## this method will return inverse of a matrix
## it calculates the inverse if it hasn't been calculated earlier
## otherwise it will return cached value
cacheSolve <- function(x, ...) {

    ## the below statement will give inverse of matrix x
    inverseMatrix <- x$getinverse()
    
    ## check if inverse has already been calculated
    if(!is.null(inverseMatrix)) {
        ## if it has been already calculated return cached value of matrix
        message("returning cached inverse")
        return(inverseMatrix)
    }
    
    ## if inverse is not calculated (i.e. inverseMatrix == NULL) then calculate
    
    ## getting matrix from specialMatrix vector x
    matrix <- x$get()
    
    ## solving for inverse of matrix
    inverseMatrix <- solve(matrix)
    
    ## setting/caching the inverse value for matrix x
    x$setinverse(inverseMatrix)
    
    ## returning the inverse of x
    inverseMatrix
}
