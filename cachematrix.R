
## This function create a Cache Matrix containning the matrix data and a cache of his inverse data

makeCacheMatrix <- function(x = matrix()) {
        ## this is a special object that stores a matrix and cache's its inverse
        inv <- NULL
        
        ## Set the data
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the data
        get <- function() x
        
        ## Set the inverse data into "inv"
        setinv <- function(i) inv <<- i
        
        ## Get the inverse data
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function return the inverse of the CacheMatrix if this one is square
## If the inverse have ever been computed then the function return the cache data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## If we can't inverse the matrix
        if(nrow(x$get())!=ncol(x$get())){
                message("can't inverte matrix")
                return(NULL)
        }
        
        i <- x$getinv()
        
        ## If we already have computed the inverse
        if(!is.null(i)){
                message("getting cached data")
                return (i)
        }
        
        ## Else we compute the inverse
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
