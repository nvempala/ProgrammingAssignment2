## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(matinverse) m <<- matinverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve first checks to see if an inverse matrix is already cached.
## If so, it retrieves the cached matrix. If not, it calles the get() function,
## stores the returned matrix into 'data', passes this into the solve() function, 
## stores the returned inverse matrix in 'm', sets the value of 'm' in the cache, gets this value and prints it out

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
