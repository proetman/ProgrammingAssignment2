## Construct an environment where a Matrix can be Inversed, and the result
## stored in memory cache. If the matrix inverse is required again, the cached
## version can be fetched, rather than recalculating it.
##
## Example usage:
##
##    # Construct a 3x3 matrix with 9 random numbers
##    myMatrix <- matrix(rnorm(9), 3,3)
##
##    # allocate block of memory for the Matrix, the inverse, and functions to
##    # set and get
##    myMatrixCache <- makeCacheMatrix(myMatrix)
##
##    # Calculate the inverse of the matrix, put inverse into variable result
##    result <- cacheSolve(myMatrixCache)
##
##    # If function is called again, inverse if fetched from cache (faster)
##    result2 <- cacheSolve(myMatrixCache)


## makeCacheMatrix
##     Setup area in memory for matrix, it's inverse, and functions
##     to set and get.

makeCacheMatrix <- function(x = matrix()) {
        # Initialise the result
        m <- NULL

        # Setup area in parent memory for matrix and empty inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # Get matrix
        get <- function() {
                x
        }

        # Push inverse matrix into "m"
        setsolve <- function(solve) {
                m <<- solve
        }

        # Get the inverse matrix from cache and return
        getsolve <- function() {
                m
        }

        # setup functions for getting and setting
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve
##     Get the inverse of the matrix.
##     If it has been previously calculated, return the cached value
##     Otherwise calculate the value, store in cache, then return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()

	# See if it has already been calculated, if so, return it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	# get the matrix, solve it, save in cache
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)

        # return the inverse.
        m
}
