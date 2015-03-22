## This function creates a special "matrix" object that can cache its inverse.

## This is very similar to the example given to us of the makeVector function (which caches a mean),
## except that this function caches the inverse of a mean.

makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL 
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL ## These lines of code are essentially the same as the makeVector function.
      }
      get <- function() x 
      setInv <- function(inv) xinv <<- inv ## Here, instead of getting the mean like in makeVector,
      ## it gets the inverse.
      getInv <- function() xinv ## Again, I use the inverse as opposed to the mean.
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
	       ## This should set the value of the vector, get the value of the vector, and 
	       ## set the value of the inverse, get the value of the inverse.
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## Again, it is very similar to the exampe given to us of the cachemean function, 
## except that it calculates the inverse of the function I created above.

cacheSolve <- function(x, ...) {
      m <- x$getInv()## I use the inverse here so that it corresponds to the above function.
      if(!is.null(m)) { 
	  message("getting cached data")
	  return(m) ## These lines of code are similar to the cachemean function.
      }
      data <- x$get() 
      m <- solve(data) ## This line should get the result
      x$setInv(m) 
      m ## This line returns the result above
 }
