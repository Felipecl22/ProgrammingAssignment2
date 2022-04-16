#The first function, makeVector creates a special "vector", which is really a list 
#containing a function to

#1 set the value of the matrix to makeCacheMatrix (x)
#2 get the value of the matrix (x)
#3 set the value of the inverse (m)
#4 get the value of the inverse (m)

# I just did the same of the example, the difference is the inverse function XD.

makeCacheMatrix <- function(x = matrix()) {
        m <- null
        set <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                m <<- inverse             
        }         
        getInverse <- function() {
                m
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
} 

cacheinverse <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        x <- x$getInverse()
        
        ## Just return the inverse if its already set
        if( !is.null(x) ) {
                message("getting cached data")
                return(x)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        x <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(x)
        
        ## Return the matrix
        x
}
