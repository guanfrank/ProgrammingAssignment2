## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL             #    Initializing local variable
    set <-
        function(y) {
            #  store the matrix to cache X, reset M to NULL
            X <<- y             # store the matrix to X
            M <<- NULL          # reset the matrix to tell when cacheSolve has run before
        }
    get <- function() X         # get the actual matrix
    setM <- function(m) M <<- m             # set the value of Mto m passed in the call to $setM
    getM <- function()  M       # get the matrix from cache to see whether it is NULL
    list(                       # list with the available functions
        set = set,
        get = get,

        setM = setM,
        getM = getM
    )
}


##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getM()               # get the cache of the matrix
    if (!is.null(m)) {          # check if this matrix has been calculated
        message("getting cached data")  # if so, prints this message "getting cached data" and
        return(m)               # returns the inverse of the matrix and skips the computation.
    }
    #If the inverse has not been calculated:
    data <- x$get()             # get the actual matrix
    inverse <- solve(data, ...)     # calculating the inverse of the matrix using solve
    x$setM(inverse)             # updating the variable that holds the inverse of the matrix
    inverse
}
