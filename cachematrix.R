## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly
## Following functions developed to cache an inverse of a matrix for reuse
## Assumption: Matrix supplied is always invertible


# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# Creates a list containing functions to do the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i   <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
  
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set=set, get=get,
         setinverse=setinverse, 
         getinverse=getinverse)
    
}


## cacheSolve function computes the inverse of the matrix passed in. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cached inverse is returned. Otherwise, inverse is calculated and saved
## cache using setinverse function


cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("Getting cached data.")
        return(i)
    }
    
    data <- x$get()
    i    <- solve(data)
    x$setinverse(i)
    i ## Return a matrix that is the inverse of 'x'
}
