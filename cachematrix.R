## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse,
# which is really:
# a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) invMatrix <<- inverse
        getInverse <- function() invMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


# This function calculates the inverse matrix of the special "matrix" 
# created with the function "makeCacheMatrix". However, it first checks 
# to see if the inverse matrix has already been calculated. If so, it 
# gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the meainverse matrix of the data and sets the 
# value of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("getting cached inverse matrix")
                return(invMatrix)
        }
        matrixData <- x$getMatrix()
        invMatrix <- solve(matrixData, ...)
        x$setInverse(invMatrix)
        invMatrix
}
