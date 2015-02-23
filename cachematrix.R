## Put comments here that give an overall description of what your
## functions do

## This function is used to create a custom 'matrix' object that saves a cache of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) 
{
	#initiaze inverse
	inverse <- NULL
	
	#Set the matrix
	set <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}
	
	#get the matrix
	get <- function() x
	
	#set the inverse
	setinverse <- function(inv) inverse <<- inv
	
	#set the inverse
	getinverse <- function() inverse
	
	#resultant matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function checks if an inverse of the matrix has already been created, then that inverse is returned. Otherwise the inverse is returned.

cacheSolve <- function(x, ...) 
{
        inverse <- x$getinverse()
		
		#check if inverse has been calculated, and return it
		if(!is.null(inverse))
		{
			message("cache hit: inverse returned")
			return(inverse)
		}
		
		#calculate fresh inverse
		invdata <- x$get()
		inverse <- solve(datainverse, ...)
		
		#save a copy of the inverse
		x$setinverse(inverse)
		
		#return
		inverse
}
