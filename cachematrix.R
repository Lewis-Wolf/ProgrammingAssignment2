## This function is meant to first create a matrix out of the provided data and then solve for the inverse of 
## that matrix, which is then cached.


makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinmatrix <- function(solve) m <<- solve
        getinmatrix <- function() m
        list(set = set, get = get,
             setinmatrix = setinmatrix,
             getinmatrix = getinmatrix)
}


## This function is meant to draw from the cached matrix from the 
## makeCacheMatrix function and then solve it. If a matrix is not
## already cached, it calculates it and prints it.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinmatrix(m)
        m
}
