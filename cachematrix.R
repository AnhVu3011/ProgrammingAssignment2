# These functions, "makeCacheMatrix" and "cacheSolve", are able to cache the inverse of a matrix

# "makeCacheMatrix" is a function that creates a special "matrix" object that can cache its inverse given a 
# square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        # set value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get value of the matrix
        get <- function() x

        # set value of inverse matrix
        set_inverse <- function(inverse) i <<- inverse
        
        # get value of inverse matrix
        get_inverse <- function() i

        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


# "cacheSolve" computes the inverse of the special "matrix" created by the "makeCacheMatrix" function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inverse()
        
        # check if the inverse has already been calculated. If so, retrieves it from the cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i) 
        }
        
        # otherwise, calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$get_inverse(i)
        i
}