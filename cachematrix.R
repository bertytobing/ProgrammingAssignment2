## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly
## Assumption: matrix is always invertible

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. contains
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
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

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cache data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setinverse(inv)
	inv
}

# Testing
# mat <- makeCacheMatrix(replicate(100, rnorm(100)))
# mat$getinverse()
# 	NULL
# cacheSolve(mat)
#	printed inverse matrix
# mat$getinverse()
#	printed inverse matrix
# cacheSolve(mat)
#	printed "getting cache data"
# x <- replicate(4, rnorm(4))
# x
# 	           [,1]       [,2]       [,3]       [,4]
#	[1,]  0.6554616  0.1685899 -0.5269790  1.4775357
#	[2,]  0.3705502 -2.2565825  0.2679818  0.8472762
#	[3,] -0.4214621 -1.0691667 -0.9368014 -0.4881329
# 	[4,] -1.3947607 -0.8951239  1.4994568  0.6226581
# mat2 <- makeCacheMatrix(x)
# cacheSolve(mat2)
#            [,1]        [,2]        [,3]        [,4]
#	[1,] -0.2364361  0.46685555 -0.57988845 -0.52882218
#	[2,]  0.1496472 -0.36596873 -0.15632218  0.02033473
#	[3,] -0.3902987  0.24775542 -0.65059264  0.07899575
#	[4,]  0.6254106 -0.07698314  0.04304484  0.26044954
# cacheSolve(mat2)
#	getting cache data
#           [,1]        [,2]        [,3]        [,4]
#	[1,] -0.2364361  0.46685555 -0.57988845 -0.52882218
#	[2,]  0.1496472 -0.36596873 -0.15632218  0.02033473
#	[3,] -0.3902987  0.24775542 -0.65059264  0.07899575
#	[4,]  0.6254106 -0.07698314  0.04304484  0.26044954