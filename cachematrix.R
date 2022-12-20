## Put comments here that give an overall description of what your
## functions do

# There are two functions described here: makeCacheMatrix and cacheSolve. makeCacheMatrix creates a special
# 'matrix' and then caches its inverse. cacheSolve computes the special 'matrix' that is returned by the makeCacheMatrix function.

## Write a short comment describing this function

#makeCacheMatrix creates a special 'matrix' and then caches its inverse. This special matrix contains the 
#functions to set the value of matrix, get the value of matrix, set the value of inverse, and get the 
#value of inverse.
get

makeCacheMatrix <- function(x = matrix()) { #Defining the function here with default matrix
  
  i <- NULL       # initialized the value of i as NULL and this will hold the value of inverse 
  set <- function(y) {  
    x <<- y  
    i <<- NULL
  }
  get <- function() x  # function to get matrix x
  setinverse <- function(inverse) i <<- inverse  # assign value of inverse 'i' in the parent environment
  getinverse <- function() i # get the inverse of matrix
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

# cacheSolve computes the special 'matrix' that is created with the makeCacheMatrix function. It checks if the inverse has already 
#been calculated. If yes, then it gets the inverse from the cache and skips computation. Otherwise, it calculates the 
#inverse of the data and sets the value of the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {  #check if inverse is null
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)  # calculates inverse value
  x$setinverse(i)
  i  #return a matrix that is inverse of x
}
