## Creates the "Special" matrix that caches the inverse matrix
## It returns a list object with:
## $get() function that returns original data matrix
## $set(y) procedure to set new matrix "y" to the data
## $getSolve() function returning inverse matrix if already calculated, NULL otherwise
## $setSolve(y) procedure to set the inverse matrix "y" and cache it
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL  # inverse cache, initially NULL
    set <- function(y) {  # when assigning new data, save it and clear cache
        x <<- y
        s <<- NULL
    }
    get <- function() x  # just returns data
    setSolve <- function(solve) s <<- solve  # assign new solution to cache
    getSolve <- function() s  # returns cache
    list(set = set, get = get,  ## returns object with previous functions
         setSolve = setSolve,
         getSolve = getSolve)
}


## Return a matrix that is the inverse of 'x' using cache
## If solution is already calculated, returns it, otherwise calculates, stores and returns solution
cacheSolve <- function(x, ...) {
    s <- x$getSolve() # reads cache
    if (!is.null(s)) { # already calculated?
        message("getting cached data")
        return(s) # returns cache
    }
    data <- x$get() # not calculated before, so get matrix data to do it now
    s <- solve(data, ...)
    x$setSolve(s) # store in cache
    s # and returns value
}
