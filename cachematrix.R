## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## <<- searches for a variable in the parent envirnments for assignment and if
##      not found, will assign the variable in the global environment.  

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        
        list(set = set, get = get, 
                setInverse = setInverse, getInverse = getInverse)
       
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {

        inverse <- x$getInverse()
        
        if(!is.null(inverse)) {
                message("Getting chased data...")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
        
}
