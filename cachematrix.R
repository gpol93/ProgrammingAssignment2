
#the goal of this function is to create a special object (a matrix in this particular case) that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                 #initializing a mtrix filled later
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,                               #returns a list
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#this second function calcualtes the inverse of the cache matrix (computed thanks to function above)
cacheSolve <- function(x) {
  cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {                            #is inverse matrix already calculated then get the inverse from the cache
      message("get cache data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
  }
}
