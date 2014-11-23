
## Author: Devon Smith
## Date Created: 2014-11-23

## This is a pair of functions -- "makeCacheMatrix" and "cacheSolve" -- which
## are used to create a matrix 'object' that can calculate and cache its
## inverse. The object returned by "makeCacheMatrix" can be passed to
## "cacheSolve" which will either calculate and cache the inverse, or simply
## return a previously cached inverse. The cached inverse is erased when a new
## value is supplied with the "set" function, so that a new inverse will be
## calculated the next time it's called.

## Input: A matrix 
## Output: A list/object that stores the input matrix and its inverse
## Description: Creates a list with functions to set and get a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Input: A matrix created by "makeCacheMatrix"
## Output: The inverse of the the input matrix
## Description: If matrix inverse has been cached, the cached version will be returned,
##              otherwise the inverse will be calculated, cached in the input object, then returned
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
