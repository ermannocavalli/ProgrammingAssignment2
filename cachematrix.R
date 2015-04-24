## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseX <- matrix(numeric(0), 0, 0)
        
        set <- function(y) {
                x <<- y
                inverseX <- matrix(numeric(0), 0, 0) 
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inverseX <<- inverse
        
        getInverse <- function() inverseX
        
        list (set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}        
        
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if ((nrow(i) > 0) && (ncol(i) > 0)) {
                message("getting cached data")
                i
        } else {
                message("computing inverse and setting cache")
                x$setInverse(solve(x$get()))
                i <- x$getInverse()
        }
        
}
