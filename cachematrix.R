## The functions 'makeCacheMatrix' and 'cacheSolve' cache the inverse
## of a matrix.

## 'makeCacheMatrix' is a function that creates a special 'matrix' object
## that can cache its inverse. It is a list containing four functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverseMatrix) inv <<- inverseMatrix
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' is a function that returns the inverse of the matrix. 
## First, 'cacheSolve' tests if the inverse of the matrix is cached. 
## If the inverse of the matrix is cached, 'cacheSolve' returns the
## cached data indicated by printing the text 'getting cached data'. 
## If the inverse of the matrix is not cached, 'cacheSolve' calculates, 
## caches and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                print("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
