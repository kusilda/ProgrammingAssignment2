## Put comments here that give an overall description of what your
## functions do

# Functions will return inverse matrix to the one user feeds in the makeCacheMatrix()
# If inverse matrix was already used and stored in cache, function will 
# take it straight from the chache avoiding calculations. If matrix is used first 
# time then inverse matrix will be calculated and stored in cache for case the
# same matrix will be used next time again. 
# Example of use:
# x =  makeCacheMatrix(matrix(5:8,2));
# cacheSolve(x);


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# Defines functions to use with the matrix x
	# Returns list of these functions

	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseM <- function(inversem) m <<- inversem
        getInverseM <- function() m
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
	  # First it checks if inverse matrix already exists and if yes then 
	  # cached data will be returned; if not then inverse matrix will 
	  # be calculated
 
	  m <- x$getInverseM()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseM(m)
        m

}
