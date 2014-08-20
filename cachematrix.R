## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # slv contains solve() result, is reset every time when new matrix created
        slv <- NULL  

        set <- function(y) {
                x <<- y
                slv <<- NULL
        }
        # returns the value of original matrix
        get <- function() x  

        # called by cashe function during the first call to save solve() result
        setsolve <- function(solve) slv <<- solve
        
        # return cahed value 
        getsolve <- function() slv
        
        # lists all the functions of object, needed for functions being accessed externally
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { # input is an object created by makeCacheMatrix()
        # gets slv value from x
        slv <- x$getsolve()
        
        # if slv was already cached
        if(!is.null(slv)) {
                message("getting cached data")
                
                # return cached value
                return(slv)
        }
        
        # if slv was not cached
        # get matrix from object
        data <- x$get()
        # calculate inverse with solve()
        slv <- solve(data, ...)
        # store calculated matrix
        x$setsolve(slv)
        slv ## Return a matrix that is the inverse of 'x'
}
