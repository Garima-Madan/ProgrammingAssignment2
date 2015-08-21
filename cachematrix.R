## The given pair of functions creates a special matrix object and computes its inverse
## If the inverse has already been computed, it is retrieved from the cache rather than being recomputed

## This function creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the matrix object created using above function and chaches it if it already exists


cacheSolve <- function(x, ...) {
        
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv

}
