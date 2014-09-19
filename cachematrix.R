# The makeCacheMatrix and casheSolve functions provide a computationally
# efficent way to handle computing the inverse of a matrix.  The  efficiency 
# is achieved by creating a cache of the matrix inverse that can be accessed 
# for future use.  First use makeCacheMatrix() to make a special matrix object
# and then call cacheSolve() with the special matrix as an argument to use
# the cached matrix.  The matrix inverse will be calculated on the first run
# of cacheSolve().  Subsequent calls to cacheSolve for an instance of an 
# makeCacheMatrix() object will return the cached matrix inverse.  

# makeCacheMatrix() creates a special matrix object with the following 
# functions: set() to initialize the special matrix; get() to retreive
# the value of the matrix; setInverse() to use the solve() function to 
# calculate the matrix inverse; and getInverse() to retrieve the 
# matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        # Initalizes a new matrix and inverse. 
        # Note: The variables reference variables in the parent environment. 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # Returns the value of the original matrix.   
        get <- function() x
        
        # Calculates in matrix inverse.
        # Note: The inverse references variables in the parent environment. 
        setInverse <- function(solve) inverse <<- solve
        
        # Returns the values of the matrix inverse.  
        getInverse <- function() inverse
        
        # Returns a list of the four function defined in makeCacheMatrix.
        list(set = set, get = get, 
                setInverse = setInverse, getInverse = getInverse)
       
}

# This function allows access to the calculated and cached inverted matrix.
# More specically, the function checks if the inverted matrix has been 
# calculated and returns the cached inverted matrix.  Otherwise the inverted 
# matrix is calculated.  

cacheSolve <- function(x, ...) {

        # Gets the current value of inverse as defined in makeCacheMatrix. 
        inverse <- x$getInverse()
        
        # If the matrix inverse has been cached, the function returns that 
        # value and then the function ends.  
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        
        # If the matrix inverse has not been calculated, the calculation is
        # completed and then returned.  
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
        
}
