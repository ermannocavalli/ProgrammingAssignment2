## Put comments here that give an overall description of what your
## functions do

## makecachematrix creates a cache that contains
## a matrix and its inverse.
## The cache can be manipulated with four functions:
## - set(): sets a matrix into the cache
## - get(): gets the matrix from the cache
## - setInverse(): sets the inverse of a matrix into cache
## - getInverse(): gets the inverse of a matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        ## start setting the inverse to a matrix with
        ## zero rows and zero columns.
        ## this will be used to check for empty cache
        inverseX <- matrix(numeric(0), 0, 0)
        
        ## set() sets a new matrix into cache
        ## and resets inverse
        set <- function(y) {
                x <<- y
                inverseX <- matrix(numeric(0), 0, 0) 
        }
        
        ## get() returns matrix from cache
        get <- function() x
        
        ## setInverse() sets matrix inverse into cache
        setInverse <- function(inverse) inverseX <<- inverse
        
        ## getInverse() gets matrix inverse from cache
        getInverse <- function() inverseX
        
        ## list() lists all available functions
        list (set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}        
        
## cacheSolve() takes in input a matrix and checks if its
## inverse is already loaded in cache. It returns an
## inverse if there is one in the cache, otherwise
## it computes the inverse and sets into the cache

cacheSolve <- function(x, ...) {
        ## check if there is already an inverse matrix
        ## in cache
        i <- x$getInverse()
        if ((nrow(i) > 0) && (ncol(i) > 0)) {
                ## there is an inverse in cache:
                ## return that inverse
                message("getting cached data")
                i
        } else {
                ## no inverse in cache: an inverse is
                ## computed using solve(), loaded in 
                ## cache using setInverse() and then
                ## returned using getInverse()
                message("computing inverse and setting cache")
                x$setInverse(solve(x$get()))
                i <- x$getInverse()
        }
        
}
