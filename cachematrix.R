## Caching the inverse of a Matrix
## This functions cache the inverse of a matrix rather than compute it repeatedly.


## Clean cache if the matrix is not equal to the last has already been calculated.
## Cache the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	
	matequal <- function(z, t) is.matrix(t) && is.matrix(z) && dim(t) == dim(z) && all(t == z)

	if ( is.null(matrice) || !matequal(x,matrice)){
		m <- NULL
	}
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverseMatrix <- function(matriceInversa) {
		m <<- matriceInversa
		matrice <<- x 
	}
	getInverseMatrix <- function() m
	list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Return a matrix that is the inverse of 'x' using cache

cacheSolve <- function(x, ...) { 

	m <- x$getInverseMatrix()

	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverseMatrix(m)
	m

}
