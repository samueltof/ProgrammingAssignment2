## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    inv_mat <- NULL  # Initial inverse matrix
    # Set matrix
    set <- function(mat) {
        x <<- mat         
        inv_mat <<- NULL
    }
    
    # Get the matrix
    get <- function() {
        x
    }
    
    # Set inverse matrix
    setInverseMat <- function(inv) {
        inv_mat <-- inv
    }
    
    # Get inverse matrix
    getInverseMat <- function() {
        inv_mat
    }
    
    return( list(set=set, get=get, setInverseMat = setInverseMat, getInverseMat=getInverseMat ) )

}


## Write a short comment describing this function
# Get inverse from the cache if the inverse of a matrix was calculated but did not changed.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getInverseMat()
    # Test if it already exists
    if (!is.null(inv_mat)) {
        message('getting cache data')
        return(inv_mat)
    }
    # Get matrix
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$setInverseMat(inv_mat)
    # Return inverse matrix
    return(inv_mat)
}
