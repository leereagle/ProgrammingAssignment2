makeCacheMatrix <- function(x = matrix()) {
    ## return: a list containing functions to
    ## 1. set the matrix
    ## 2. get the matrix
    ## 3. set the inverse
    ## 4. get the inverse
    ## this list is used as the input to cacheSolve()
    
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinv = function(inverse) m <<- inverse 
    getinv = function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    m = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(m)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(m)
    }
    
    # otherwise, calculates the inverse 
    mat.data = x$get()
    m = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(m)
    
    return(m)
}
