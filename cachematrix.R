##Indrajit Choudhury 04/06/16 R Programming Assignment 2 

##The purpose of this project is to cache the Inverse of a Matrix
##The functions below create a special object to store a matrix
##and the other to find its inverse. 

##This function creates a matrix object, whose inverse can be 
##cached 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##This function computes the inverse of the matrix object created
##by makeCacheMatrix function and if it has already been computed
##then it should just retrieve the value of the inverse from the cache 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv     
}
