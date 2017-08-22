# These 2 functions allow us to use a cached version of a calculation (the inverse of a matrix) if it has previously
# been calculated.  

# The first function accepts a matrix object and create a list with four functions.
# The four function will: set the value of the matrix object, 
# get the value of the matrix object, set the value of the inverted matrix, 
# get the value of the inverted matrix. The list returned can then be passed to cacheSolve(), the second function. 
makeCacheMatrix <- function(x = matrix()) { 
  ## Set the value of the matrix inverse to NULL
  MatrixInverse <- NULL                     
  ## Create a function which has 2 elements: the first takes the matrix value, the second takes the calculation result
  set <- function(y) {                      
    x <<- y
    ## Start with the matrix value as NULL.
    MatrixInverse <<- NULL              
  }
  ## calculate and store the value of the matrix inverse
  get <- function() x                           
  #calculates the inverse using solve
  setInverse <- function(solve) MatrixInverse <<- solve 
  # gets the inverse     
  getInverse <- function() MatrixInverse        
  ## creates a list with the values        
  list(set = set, get = get,                    
       setInverse = setInverse,
       getInverse = getInverse)
}

# Pass the list returned from makeCacheMatrix() to this function to check if there is an already calculated inverse 
# and return it,  otherwise calculate it, store it and return it.
cacheSolve<- function(x, ...) {    
 # message(x$getInverse)
  MatrixInverse <- x$getInverse()
  #Check whether the MatrixInverse exists or is NULL
  if(!is.null(MatrixInverse)) {                 
    message("getting cached data")
    #If there is a cached result available, then use it and message the user
    return(MatrixInverse)
  }
  #if the inverse if not there, then calculate it and store it for next time, then return the result.
  data <- x$get()                               
  MatrixInverse <- solve(data, ...)
  x$setInverse(MatrixInverse)
  MatrixInverse
}
