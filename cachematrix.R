## This function caches the inverse of a matrix. 
## It exists of two parts: 
## 1. makeCacheMatrix. The first function creats a special "matrix" object that can cache its inverse.
## 2. cacheSolve. The second function computes the inverse of the speical "matrix" returned by the makeCacheMatrix above. If the inverse has
##      already bean calculated, and the matrix has not changed, than the cachesolve should retrieve the inverse from the cache. 


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}