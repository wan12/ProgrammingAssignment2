## function creates a special “matrix” object that can cache its inverse
## function is a square invertible matrix
## return: a list containing functions to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
        x <<- y
        inv<<-NULL    ##`<<-`assigns a value to an object in an enviroment different from the current enviroment
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the “matrix” returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of the original matrix input to makeCacheMatrix()
        inv = x$getinv()
## if the inverse of the matrix has already been calculated
        if (!is.null(inv)){
##get it from the cache and skip the computation. 
                message("getting cached data")
                return(inv)
        }
## otherwise calculates the inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
         x$setinv(inv)     ##setinv function is used to set the inverse of the matrix in the cache
         return(inverse)
}

