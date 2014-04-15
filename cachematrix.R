## Put comments here that give an overall description of what your
## functions do

## The first functionm makeCacheMatrix vreates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

 	s <- NULL
        set <- function(y) {
                    x <<- y
                    s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        matrix(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


	s <- x$getinverse()
            if(!is.null(s)) {
                    message("getting cached data")
                    return(s)
            }
            data <- x$get()
            s <- solve(data, ...)
            x$setinverse(s)
            s
}
