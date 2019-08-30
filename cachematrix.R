## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y  ## assigns 'y' to 'x' in the parent environment to reset value if existing cache exists
                i <<- NULL ## assigns 'i' to 'NULL' in the parent environment
        }
        get <- function() x  ## retrieves x from parent environment
        setmatrix <- function(solve) i <<- solve  ## assigns input argument for 'solve' to 'i' from the parent envrionment
        getmatrix <- function() i
        list(set = set, get = get,  ## assigns each function to a list
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getmatrix()
        if(!is.null(i)) {    #checks to see if 'i' is null, if so returns cached value, if not, runs solve below
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setmatrix(i)
        i
}
