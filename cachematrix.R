## Angel Castillo
## Functions to obtain and cache the inverse of a inversable matrix


##makeCacheMatrix This function creates a special "matrix" object that can cache its inverse
##does so by using lexical scoping to access the object x outside the makeCacheMatrix function to store the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<-y
        m <<- NULL        
    }
    get <- function() x
    setinverse <- function(solve) m <<-- solve
    getinverse <- function() m
    list (set = set, get = get,
          setinverse = setinverse
          getinverse = getinverse)
}


## cacheSolv This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## This is done by trying to pull x$getinverse from the x object into m. If that is null then 
## the inverse has not been previously calculated. if it's not null then the inverse was calculated 
## previously and is now stored in the value of m. 
## Otherwise the variable data gets the matrix from x and then using the solve function the 
## inverse is obtained and stored into m. Then this inverse is stored into the x object so that it's 
## available for future queries. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)            
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m    
}
