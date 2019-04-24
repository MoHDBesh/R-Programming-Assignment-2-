
##this function will take advantage of the scoping rules of the R language 
## by give the mean  without needing to calculate it of we already calculate it  
makeCacheMatrix <- function(x = matrix()) {
        m <- NUL
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

##this function will take advantage of the scoping rules of the R language 
## by give the inverse without needing to calculate it of we already calculate it  


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
