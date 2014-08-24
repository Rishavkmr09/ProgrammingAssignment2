##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix())
   {
    i <- NULL

    ##Set the matrix
    set <- function(mat) 
    {
            m <<- mat
            i <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse)
    {
        i <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function()
    {  
        i
    }

    ## Return 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

    }


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.

cacheSolve <- function(x, ...)
   {
    ## Returns inverse of 'x'
    m <- x$getInverse()
    ## Checking if inverse is already set
   if( !is.null(m) ) 
    {
            message("getting cached data")
            return(m)
    }
     
    ## Getting the Matrix from object
    data <- x$get()
    ## Caculating Inverse via Matrix Multiplication
    m <- solve(data) %*% data
    ## setting inverse to the object
    x$setInverse(m)
     ## Returning the final solution matrix
    m
}
