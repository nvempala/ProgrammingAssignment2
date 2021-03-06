## The two functions cache the inverse of a matrix and either (a) retrieve this value from the cache, or
## (b) create the inverse freshly and retrieve it.

## The function makeCacheMatrix is definted with an argument of matrix.
## Within the function, 'm' is set to NULL locally. Within the 'set' subfunction x is assigned the values
## of the argument, and 'm' is set to NULL globally. get function retrieves the matrix.
## setInverse updates the inverse of the matrix to the cache, getInverse retrieves it.

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
