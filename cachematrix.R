Q## We have two functions for caching the inverse of a matrix
## 1. makeCacheMatrix creates a list of functions to 
##   set and get the input matrix, and set the inverse and get the inverse of the input matrix and get the matrix
##
## 2. cacheSolve function creates the inverse of the input matrix for the first run and return the cached inverse in all subsequent runs
##   for a given matrix.  

##	This function also checks for non-square matrix input and return a message stating there is no inverse
##	This function also works with a 1x1 matrix 

makeCacheMatrix <- function(x=matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
			
	  }
        get <- function() x
	  setinverse <- function(solvem) m <<- solvem
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x=matrix(), ...) {        
	if ( nrow(x$get()) != ncol(x$get()) )
	{
		message("NOT A SQUARE MATRIX - THERE IS NO INVERSE!")
		message("INPUT MATRIX IS: ")
		return(x$get())
	}
	m <- x$getinverse()
      
	if(length(m) == length(x$get()) & !is.na(m[1,1])) 
	{
      	message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
