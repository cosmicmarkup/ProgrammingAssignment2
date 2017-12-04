## This function creates a special "matrix" object (which assumes it's always invertible) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	# Set the value of the vector
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	# Get the value of the vector
	get <- function() x
	
	# Set the value of the inversed matrix
	setinverse <- function(inverse) m <<- inverse
	
	# Get the value of the inversed matrix
	getinverse <- function() m
	
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function returns the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value 
## of it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    
    if(!is.null(m)) {
    	message("getting cached data")
    	return(m)
    }
    
    data <- x$get()
    
    m <- solve(data)
    
    x$setinverse(m)
    
    m
}
