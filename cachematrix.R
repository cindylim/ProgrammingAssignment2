## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.
## The "cacheSolve" function computes the inverse of the special "matrix" returned by "makeCacheMatrix".

## The "makeCacheMatrix" function is a list containing functions to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) m <<- inverse
		getInverse <- function() m
		list(set=set, get=get,
			setInverse = setInverse,
			getInverse=getInverse)
}


## The "cacheSolve" function first checks to see if the inverse of the matrix has already been calculated..
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInverse()
		if(!is.null(m)) {
				message("getting cached data")			
				return(m)
		}
		data <- x$get()
		m <- solve(data,...)
		x$setInverse(m)
		m

}
