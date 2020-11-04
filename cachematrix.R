##Two functions are used to create a special object that stores a square matrix
## and caches its inverse

## The makeCacheMatrix creates a special "vector" which is a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                x <<- y
                m <<- NULL
          }
          get <- function() x
          setinv <- function(inve) m <<- inve
          getinv <- function() m
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}





## cacheSolve function calculates the inverse of the special matrix
##  It first checks if the inverse has already been calculated
## If so it gets the inverse from the cache and skips computations
## otherwise it calculates the inverse and sets the value of the inverse via the setinv function

cacheSolve <- function(x, ...) {
         m <- x$getinv()
         if(!is.null(m)) {
                message("getting cached data")
                return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setinv(m)
         m
}
