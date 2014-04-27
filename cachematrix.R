## These functions allow for the cacheing of a potentially time-consuming
## computation of matrix inversion.  The inverse of a matrix can be cached and 
## recalled later as needed without having to recompute it.

## This function creates a special matrix and sets the cached inverse to null, 
## then it returns a list with the following functions as list values:
## set - sets the matrix data and resets inv to NULL
## get - gets the matrix data
## setinv - sets the inverse of the matrix
## getinv - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function takes as input a special matrix created by the MakeCacheMatrix
## function.  It checks to see if the inverse has already been cached and if so
## retrieves the inverse from the cache skipping the computation.  Otherwise it 
## computes the inverse of the matrix and sets the value of the inverse in
## cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
