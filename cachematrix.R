## Two functions whihc return the inverse of matrix from the the chache if available  or calculates and returns the inverse.`


##makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<-inverse} ## Set the inverse
  getInverse <- function() {inv} ## Get the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ##Solve is used to calculate the inverse
  x$setInverse(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
