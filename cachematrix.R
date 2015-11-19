##
## <use tabs>
## 

## 

##
##	Special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(matrix = matrix())
{
	inverse_matrix <- NULL
	
	## Setter for matrix
	## Note: setting a new matrix, the inverse of matrix is cleared.
	set	 <- function (original_matrix) {
		matrix <<- original_matrix
		inverse_matrix <<- NULL
	}
	
	## Getter for matrix (original matrix)
	get <- function () {
		matrix
	}
	
	##  Setter for inverse of matrix
	setinverse <- function (mi) {
		inverse_matrix <<- mi	
	}
	
	## Getter for inverse matrix
	getinverse <- function () {
		inverse_matrix
	}
	
	## definition of available "methods"
	list(
		set=set,
		get=get,
		setinverse = setinverse,
		getinverse = getinverse
	);
		

}



##
## This function calculate the inverse of a matrix and cached it
##
## + 'cachematrix' must be a matrix created with makeCacheMatrix
##
cacheSolve <- function(cachematrix, ...)
{
	## try to recover the inverse from cache
	matrix_inverse <- cachematrix$getinverse()

	if (is.null(matrix_inverse)) {
		## Opps.. there is no cache. Need to calculate the inverse.
		message("Calculating inverse of matrix...", appendLF=FALSE)
		## First, i need to recover the original matrix
		matrix <- cachematrix$get()
		
		## For this assignment, assume that the matrix supplied
		## is always invertible.
		## --> Then i solve it to get its inverse matrix
		matrix_inverse <- solve(matrix, ...)
		
		## ...after calculate the inverse, we saved it into cache.
		cachematrix$setinverse(matrix_inverse)
		
		message("Done")
		
	} else {
		## Great, i save some time
		message("Getting inverse from cache")
	}

	## returns
	matrix_inverse
}
