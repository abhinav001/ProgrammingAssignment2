## These two functions provide a mechanism to cache 
## the results of a matrix inversion to avoid expensive computations 
## if the same inversion is required repeatedly. It is accomplished by 
## creating a special set of functions that allow checking whether the inversion
## was carried out and cached earlier. If yes the result is fetched from the cache
## else it is computed normally and then cached for the next time it may be needed.

## THis function
 	
makeCacheMatrix <- function(x = matrix()) {
 	inverted <- NULL				## inverted would contain cached value
	set <- function(y) {                            
		x <<- y
		inverted <<- NULL
	}
	get <- function() x
	setinverted <- function(solved) inverted <<- solved	## solved contins the result of matrix inversion
	getinverted <- function() inverted
	
	list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}
 	
	
	
## Write a short comment describing this function
	
cacheSolve <- function(x, ...) {
 	        ## Return a matrix that is the inverse of 'x'
			
	inverted <- x$getinverted()
	if (!is.null(inverted)) {
		message("getting cached data")
		return(inverted)
	}
	data <- x$get()
	solved <- solve(data, ...)
	x$setinverted(solved)
	inverted <- x$getinverted()
	inverted
}
