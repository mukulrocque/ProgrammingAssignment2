## Put comments here that give an overall description of what your
## functions do
## R function to cache potentially time consuming operations
## using superassignment variable <<- to understand lexical scoping
## Caching the inverse of a MAtrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## function to create an object
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## checks if inverse is present (also if matrix changed m <- NULL)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## executed only if m == NULL implies matrix changed or inverse wasn't present
        data <- x$get()
        ##compute inverse
        m <- solve(data, ...)           
        ## set inverse using super assignment
        x$setinverse(m)
        ## return calculated inverse
        m
}
