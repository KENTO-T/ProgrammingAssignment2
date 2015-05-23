makeCacheMatrix <- function(x = matrix()) { #substitute matrix x for the function
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) { #... through x another functions
        m <- x$getinverse()
        if(!is.null(m)) { #when mean is cached 
                message("getting cached data") #this message print
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
