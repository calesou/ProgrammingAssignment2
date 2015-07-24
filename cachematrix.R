## The function makeCacheMatrix creates a special vector with 3 functions (get, setinv, getinv) used to get the matrix
## stored in the function, set the m´s value if it is not set yet and get m if it is already stored

makeCacheMatrix <- function(x = matrix()) {
        ## sets m to NULL when the funtion is run
        m <- NULL
        ## set <- function(y) {
        ##        x <<- y
        ##        m <<- NULL
        ##}
        ## function get - pass the matrix stored to the calling function
        get <- function() x
        ## function seting - set m´s value (inverse matrix) within the context of this function (<<-) so creating the cache
        setinv <- function(solve) m <<- solve
        ## function getinv - to pass m to the calling function
        getinv <- function() m
        ## creates the list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The function cacheSolve calculates the inverse of the matrix stored in the function MakeCacheMatrix
## if it was not calculated yet. If it was already calculated, it gets the cache from MakeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## gets m from MakeCacheMatrix
        m <- x$getinv()
        ## if not null, then the inverse is restored from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## else it is calculated and stored in MakeCacheMatrix function
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
