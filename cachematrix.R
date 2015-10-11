## Put comments here that give an overall description of what your 
## functions do

## Below are the two functions that are used to create a special 
## object that stores a matrix and cache's its inverse.

## Write a short comment describing this function

## makeCacheMatrix uses scoping rules to create and store a special matrix.
## This function is a list containing functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to get the value of the matrix
    get <- function() x
    
    # Function to set the value of the inverse of matrix
    setinv <- function(inverse) inv <<- inverse
    
    # Function to get the value of the inverse of matrix
    getinv <- function() inv
    
    # List containing above functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## The function calculates the inverse of the special matrix 
## created with the above function. However, it first checks 
## to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # Check whether inverse is already calculated.
    # If yes, then display the message and return the inverse of the matrix from cache.
    # Function ends here.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If no, then get the value of the matrix 
    data <- x$get()
    # Calculate the inverse of the matrix and sets the value of the inverse in cache
    inv <- solve(data, ...)
    x$setinv(inv)
    # Function ends with displaying the inverse matrix
    inv
}
