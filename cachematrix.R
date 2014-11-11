
## function makeCacheMatrix generates a list of functions to be used by function cacheSolve. 
## It takes the input "x" as a matrix.
## function get() calls the origianl input matrix "x".
## function getinverse() applies the inbuild function solve() on the matrix "x".

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  get <- function() {x}
  getinverse <- function() {m <- solve(x)}
  
  list(get = get, getinverse = getinverse)
}



## function cacheSolve() extracts the list of functions in output "y" ,the output of function 
## makeCacheMatrix(), and check whether the function cached in y$getinverse() returns a value
## or NULL. 

## If it returns NULL, the function proceed to extract the original matrix "x" by calling the
## function cached in y$get() and compute the inverse of matrix "x" using the inbuild function
## solve(). 

## If the y$getinverse() returns a value, then it will print the message "getting cached data"
## in console and return the value instead.

cacheSolve <- function(y, ...) {
  
  m <- y$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- y$get()
  m <- solve(data, ...)
  m

}

