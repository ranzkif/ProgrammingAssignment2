## This program composed of two functions:
## (1) Function that will perform a special matrix object that can cache its inverse.
## (2) Function that computes the inverse of the special "matrix" returned by makeCacheMatrix. 

## Function that will perform a special matrix object that can cache its inverse.

## Assigned matrix function to makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
 m<- NULL

  set <- function(y) {
   x <<- y    # Set the value of x
   m <<- NULL # Clear the cache
      }
 
   get <- function() x  #Defining the value of the matrix
      setInverse <- function(inverse) m <<- inverse
  
       getInverse <- function() m  #Defining the function to get the inverse value
 
# Returning a list of the functions
 list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## Function that computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse() #Get the cached value for the inverse
  if(!is.null(m)) { # Detetmine if the cache was not empty otherwise return the m
    message("getting cached data")
    return(m)
  }
  
  ## Return a matrix that is the inverse of 'x'
  #Formula that will calculate the inverse and cache the result and the return the value.
  data <- x$get()  
  m <- solve(data) 
  x$setInverse(m)  
  m                
}
