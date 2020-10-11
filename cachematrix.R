## Put comments here that give an overall description of what your
## functions do

## The inverse of a matrix is cached using two functions : makeCacheMatrix
## and cacheSolve. We assume, that the matrix supplied is always invertible.

## I took from an example makeVector function, changed name and set 
## x as a matrix as indicated in description. m and mean are changed to inv [invertion] 
## and is set NULL in order to clear any value of inv that had been cached prior.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve()
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns an inverted matrix from cache if it was cached,
## or calculates an invertion if nothing was cached.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) { ## checking if there is chached invertion
    message("getting cached data")
    return(inv)       ## returning chached invertion
  }
  data <- x$get()   ## if no invertion is cached, it is calculated
  inv <- solve(data, ...)
  x$setinv(inv)      ## and cached for the later use
  inv
}

        ## Return a matrix that is the inverse of 'x'
