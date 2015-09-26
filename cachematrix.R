## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) # anonymus function looks like a constructor
    {
       x <<- y
       m <<- NULL
    }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get, setsol = setsol, getsol = getsol) #looks like a list mapping internal functions to be public
}


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {  # make sure x is a cacheMatric object or the call will fail
    m <- x$getsol()     # Look up precomputed solution using getsol 
    if(!is.null(m)) {   # if solution is returned designated by m NOT Equal null
      message("getting cached data")  # add diagnostic message that cache is being used
      return(m) # exit function early and return the computed value m
    }
    # By using the combination of if with return the rest of the functions is essentially the else clause
    m <- solve(x$get(), ...)  # pass the solve function the result of x get
    x$setsol(m) # set the solution of M into the x to be used later
    m  # return the value of m
}

