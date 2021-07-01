makeCacheMatrix <- function(x = matrix()){
  inv <-NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function() {x} ##obtain matrix x function used
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) { ##cache data provided
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)   ##inverse value is retrieved back
  }
  mat <- x$get()
  inv <- solve(mat, ...)  ##use for determining inverse value
  x$setInverse(inv)
  inv  ##matrix inverse of 'x' is retrieved
}
