## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve #the variable m will keep the result from the solve function
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { #if the variable m, that should contain the inverse matrix, has already the inverse it 
                          # will just return its value and the following message; otherwise it will "solve" its inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # the function solve calculates the inverse matrix 
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

