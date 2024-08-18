## The makeCacheMatrix function creates a special object that stores a matrix and can cache its inverse. Here's how it works:
# 1. x: It initializes a matrix.
# 2. inv: This variable is used to store the cached inverse of the matrix.
# 3. set(y): Updates the matrix x with a new matrix y and resets the inverse inv to NULL.
# 4. get(): Retrieves the stored matrix.
# 5. setinverse(i): Stores the inverse i in the inv variable.
# 6. getinverse(): Retrieves the cached inverse if it exists.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse  = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## The cacheSolve function computes the inverse of the matrix stored in the special object created by makeCacheMatrix. Here's what it does:
# 1. i <- x$getinverse(): It first checks if the inverse of the matrix has already been cached (stored in i).
# 2. If cached (!is.null(i)): If the inverse is available, it prints "Getting cached data ..." and returns the cached inverse without recalculating.
# 3. If not cached - it retrieves the matrix using x$get(), computes the inverse using the solve() function and stores the inverse in the cache using x$setinverse(i).
# 4. Returns: Finally, the computed inverse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data ...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
