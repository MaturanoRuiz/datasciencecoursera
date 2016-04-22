## Caching the inverse of a matrix:
# Due to matrix inversion is usually a costly computation 
# there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. The both below pair of functions
# will be used to create a sepecial object that stores a matrix 
# and caches its inverse.

## This first function creates a special matrix and cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function()	x
	setinv <- function(solve) i <<- solve
	getinv <- function() i
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This second function calculates the inverse of the special matrix
# created with the first function (makeCacheMatrix). However, it first
# checks to see if the inverse has already been calculated. If so, it gets
# the inverse from the cache and skips the computation. Otherwise,
# it calculates the inverse of the data and sets the value of the inverse
# in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix,...)
	x$setinv(i)
	i
}
