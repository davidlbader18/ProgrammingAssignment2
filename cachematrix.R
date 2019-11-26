## The first function `makeCacheMatrix`, takes a(n)
## (invertible) square matrix as its input, and
## generates a list (vector) of functions that
## allow for the implementation of caching the
## inverse of the matrix when a call to the
## second function `cacheSolve` is first made,
## using the list of functions as the input,
## and the retrieval of the cached inverse in
## subsequent calls to the second function using
## the same input (argument).

## For a given invertible square matrix, generates
## the functions that facilitate the caching, and
## retrieval of the matrix, and its inverse
## in the following function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(get=get, set=set,
       setinverse=setinverse,
       getinverse=getinverse)
}


## If the inverse is already calculated retrives it from
## the cache; otherwise calculates the inverse, and
## then caches it for future retrieval.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
