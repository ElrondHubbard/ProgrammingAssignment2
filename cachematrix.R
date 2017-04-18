## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Get value of matrix
    get <- function()x
    
    ## Set value of Inverse
    setInverse <- function(inverse) inv <<- inverse
    
    ## Get value of Inverse
    getInverse <- function() inv
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

    ## Run function - check to see if it's been solved already
    inv <- x$getInverse()
    
    ## If inverse is not null (has been solved), display cute message, print cache
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## If there is no cache (has not been solved), derive inverse, set value, cache value
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

## Series of tests to see if functions work properly
test_data <- makeCacheMatrix(matrix(1:4, 2,2))
test_data$get()
test_data$getInverse()
cacheSolve(test_data)
cacheSolve(test_data)
test_data$getInverse()
test_data$set(matrix(c(2,2,1,4),2,2))
test_data$get()
test_data$getInverse()
cacheSolve(test_data)
cacheSolve(test_data)
test_data$getInverse()

## Unless I'm really missing something, it works. 
## Final piece removes functions and test data.
rm(test_data, cacheSolve, makeCacheMatrix)
