## these two fuctions are used to calculate the inverse of a matrix by caching the
## original matrix, so that the computation can be faster
#.

## makeCacheMatrix is a function that creates the list of 4 other functions.
makeCacheMatrix <- function(x = matrix()) {
  #set i(i.e. inverse result) as null
  i <- NULL
  
  #this is used to change the original matrix and reset  i
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #this is used to get the current matrix cached
  get <- function() x
  
  #get an input(the inverse matrix) and cache it as the result of the inversion
  setinverse <- function(inverse) i <<- inverse
  
  #return the inverted matrix
  getinverse <- function() i
  
  #save the four functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this solves and returns the inverted matrix  
cacheSolve <- function(x,...) {
  
  #firstly, lets try if the inverse was already calculated and saved
  i <- x$getinverse()
  
  #if the function above provided a non null result, the inverse is provided and the function ends
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if not, it gets the original matrix (saved in matr), calculate the inverse of matr, cache
  #it as the new inverse, and return the reverse
  matr <- x$get()
  i <- solve(matr)
  x$setinverse(i)
  i
}
