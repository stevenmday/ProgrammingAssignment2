# This function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




# Compute the inverse of special 'matrix' created by 
# MakeCacheMatrix. Return cached data if matrix unchanged

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(inv)
        inv
}

