##
## Roger K. Winters
## R Programing
##

##
## Creates a special "matrix" object that can cache its inverse
##

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        ##
        ## set function which sets the non-inverted matrix
        ##
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##
        ## get function which gets the non-inverted matrix
        ##
        get <- function() x
        
        ##
        ## set and get the inverse of the matrix
        ##
        setmatrix <- function(inverse) m <<- inverse
        getmatrix <- function() m
        
        ##
        ## create a list of the functions
        ##
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


##
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
##

cacheSolve <- function(x, ...) {
        
        ##
        ## Returns a matrix that is the inverse of 'x'
        ##
        
        ##
        ## check the matrix inverse state
        ##
        m <- x$getmatrix()
        
        ##
        ## if matrix inverse has already been done, then simply return the cached results
        ##
        if(!is.null(m)) {
                message("getting cached data...")
                m <- x$getmatrix()
                return(m)  
        }
        
        ##
        ## get the matrix, calculate the it's inverse, and then cache the results; note that we assume
        ## the matrix is non-singular, eg., invertable
        ##
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        
        ##
        ## return the results
        ##
        return(m)
}
