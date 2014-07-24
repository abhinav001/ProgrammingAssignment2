## These two functions provide a mechanism to cache 
## the results of a matrix inversion to avoid expensive computations 
## if the same inversion is required repeatedly. It is accomplished by 
## creating a special set of functions that allow checking whether the inversion
## was carried out and cached earlier. If yes the result is fetched from the cache
## else it is computed normally and then cached for the next time it may be needed.

## This function creates a list of functions to operate on the input matrix and its cached inverse
 	
makeCacheMatrix <- function(x = matrix()) {
 	inverted <- NULL		## inverted would contain cached value
	set <- function(y) {
		x <<- y
		inverted <<- NULL
	}
	get <- function() x
	setinverted <- function(solved) inverted <<- solved	## solved contins the result of matrix inversion
	getinverted <- function() inverted
	
	list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}
 	
	
	
## This function checks if the inverse of the input matrix exists in cache, if yes returns the cached
## inverse, otherwise computes the inverse returns it and also caches it for the next time use.
	
cacheSolve <- function(x, ...) {
 	        ## Return a matrix that is the inverse of 'x'
			
	inverted <- x$getinverted()  ## check if the inverse is cached already
	if (!is.null(inverted)) {    ## if cached return cached value
		message("getting cached data")
		return(inverted)
	}
	data <- x$get()              ## if not cached, fetch the matrix
	solved <- solve(data, ...)   ## compute inverse of the matrix
	x$setinverted(solved)	     ## cache the inverse
	inverted <- x$getinverted()  ## fetch the cached inverse
	inverted	             ## return th einverse
}
