## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is the function used to create matrix
## setInverse, set, get, getInverse are nested within makeCachematrix used for retrieving and manipulating data in the special matrix
##cacheSolve is the function that takes the special matrix and calcuates its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## Function that creates the special matrix. Input is a matrix. Default is a empty matrix
	i <- NULL
	## Methods for setting matrix and Inverse
	set <- function(y){ ## Function for setting the matrix	
		x <<- y
		i <<- NULL
	}
	setInverse <- function(inv) i <<- inv ## function sets the inverse

	## Methods for getting Matrix and Inverse
	get <- function() x ##Get method for the matrix
	getInverse <- function() i ##Get method for the inverse
	list(set = set, setInverse = setInverse, get = get, getInverse = getInverse) ## Returns a list containing the accesing functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## Function for setting the inverse of the special matrix or retrieving it. Takes special matrix as the input
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse() ## retrieves the inverse
	if(!is.null(inv)){ ## checks if the inverse is already set or not
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setInverse(inv)
	return(inv)
	}
}
