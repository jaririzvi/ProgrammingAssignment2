## These functions in conjuction help to compute the inverse of a matrix and then cache the result,
## allowing the second function to pull from the cache quickly in the future rather than recomputing

## The first function, makeVector creates a special "vector", which is really a list containing a function to:
##	1.	set the value of the inverse of a matrix
##	2.	get the value of the inverse of a matrix
##	3.	set the value of the inverse of a matrix
##	4.	get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via setinverse function.
# This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}
