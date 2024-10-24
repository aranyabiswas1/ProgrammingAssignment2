## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Placeholder for the inverse matrix
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y     # Assigns new matrix to 'x' in the parent environment
        inv <<- NULL  # Resets the inverse cache
    }
    
    # Function to get the matrix
    get <- function() x  # Returns the matrix 'x'
    
    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse  # Assigns new inverse to 'inv'
    
    # Function to get the inverse
    getInverse <- function() inv  # Returns the cached inverse
    
    # Return a list of the above four functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve the cached inverse from 'x'
    
    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If not cached, retrieve the matrix and compute the inverse
    data <- x$get()  # Get the matrix stored in 'x'
    inv <- solve(data, ...)  # Calculate the inverse using 'solve()'
    
    x$setInverse(inv)  # Cache the calculated inverse in 'x'
    inv  # Return the inverse
}

