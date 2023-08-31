## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix works to create a matrix object and cache its inverse.
##cacheSolve, on the other hand, creates inverse of matrix made by makeCacheMatrix, 
## but in the case that it has not been already solved and the matrix has not changed,
## it will just retrieve the inverse through the cache.


## Write a short comment describing this function
## makeCacheMatrix works by setting the value of the matrix, then getting its value.
## then, it uses this information to set the value of the inverse, and then get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  
  setInvers <- function(inverse){
    inver <<- inverse
  }
  getInvers <- function() inver
  list (set = set, get = get, setInvers = setInvers, getInvers = getInvers)
}


## Write a short comment describing this function
##cacheSolve first checks if the inverse is not null when it gets it, meaning it already exists in cache
## if it is not null (therefore it already exists in cache), the function will just retrieve that inverse function and return it
## otherwise, it will get the matrix and solve and set the inverse itself, finishing off by printing the inverse 
cacheSolve <- function(x, ...) {
  inver <- x$getInvers()
  if(!is.null(x)) {
    message("getting cached data! :o")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInvers(inver)
  inver
}
