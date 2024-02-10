## The two functions "makeCacheMatrix" and "cacheSolve" create an object that stores a 
## matrix and then compute its inverse

## The first function creates a list of four helper functions
### 1. set: sets the value of the matrix, "x"
### 2. get: gets the value of the matrix, "x"
### 3. set_inv: sets the value of the inverse of "x"
### 4. get_inv: gets the value of the inverse of "x"

## The "makeCacheMatrix" function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL 
  }
  get <- function() x
  set_inv <- function(solve) inv_x <<- solve
  get_inv <- function() inv_x
  list(set = set, get = get, set_inv = set_inv,
       get_inv = get_inv)
}


## Computes the inverse (inv_x) of the special matrix returned by makeCacheMatrix 
## If the inverse has already been calculated, function retrieves the cached inverse

cacheSolve <- function(x, ...) {
   inv_x <- x$get_inv()     ## Return a matrix that is the inverse of 'x'
   if (!is.null(inv_x)) {
     message("getting cached data")
     return(inv_x)   
   }
   mat <- x$get()
   inv_x <- solve(mat, ...)
   x$set_inv(inv_x)
   inv_x
}
