## Put comments here that give an overall description of what your
## functions do

# The function 'makeCacheMatrix(x)' creates a 'spcecial matrix' that stores 
# the matrix 'x' (numeric vector) and its inverse 'inv'

# The input is the matrix 'x' and the output is a list of 
# 4 functions: set, get, setinversa and getinversa

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function () x
      setinversa <- function(solve) inv <<- solve
      getinversa <- function() inv
      list(set = set, 
           get = get, 
           setinversa = setinversa, 
           getinversa = getinversa)
}


# The function 'cacheSolve' provides the inverse matrix of 'x'
# If the inverse has already been stored, the function retrieves it
# (from the output of 'makeCacheSolve'), otherwise it is computed

# The input of cacheSolve is the output of makeCacheSolve (list of 4 functions)
# The output is the inverse matrix of 'x', providing it exists
# If the inverse exists, the function displays a message

cacheSolve <- function(x, ...) {
      inv <- x$getinversa()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinversa(inv)
      inv
}


