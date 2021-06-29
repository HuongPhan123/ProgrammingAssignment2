## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its
# inverse.
# set: set a matrix to the input matrix
# get: get the input matrix
# setinverse: set the inverse of the inout matrix by another matrix
# getinverse: get the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function( y = matric()) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(IM = matrix()) m <<- IM
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function get the inverse of  a special "matrix" object given by the previous function.

cacheSolve <- function(x, ...) {
        ## first check if the inverse has cached or not
       m <- x$getinverse()
       if(!is.null(m)) {
              message("getting cached data")
              return(m) # if yes, return the inverse matrix
       }
       # if no, compute the inverse matrix 
       mat <- x$get()
       m <- solve(mat)
       x$setinverse(m) # cache the inverse matrix
       m
}
