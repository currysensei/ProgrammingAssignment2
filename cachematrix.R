## 	These functions allow the caching of a matrix's inverse to 
##	avoid costly recalculations of the inverse
## 	makeCacheMatrix(x) creates a list that represents a matrix with its cached inverse
##	cacheSolve(x,...) returns the inverse of the matrix, but it is 
##	only calculated if it was not previously cached,

##	function: makeCacheMatrix(x = matrix())
##	
##	This function creates a special matrix object represented 
##	by a list containing four functions: set, get, setinv, getinv
##
##	Parameters: 
## 		x:		a matrix object

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, 
		 get = get,
		 setinv = setinv,
		 getinv = getinv)
}


##	function: cacheSolve(x, ...)
##	
##	This function first checks if x already contains a cached inverse of its matrix.
##	If it does, it just returns the already cached inverse, 
##	if not, it calculates it, caches it and returns it.
##
##	Parameters: 
## 		x:		a special matrix represented by a list
##				containing four functions: set, get, setinv, getinv;
##				where this structure can hold a matrix, and its inverse
##
##		...:	extra arguments to provide to solve(x, ...) function.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
