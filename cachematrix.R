## These functions are used to cache the inverse of a matrix so that
## when we need it again, it can be looked up in the cache rather
## than recomputed

## This function creates a special "matrix" object, which is a list 
## of functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created
## with the above function. It checks if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it uses solve() function to calculate the
## inverse and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	m <- x$get()
	inv <- solve(m, ...)
	x$setinv(inv)
	inv
}
