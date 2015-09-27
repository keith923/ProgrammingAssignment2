###############################################################################
## The two functions below, makeCacheMatrix, and cacheSolve, work together to 
## make repeatedly finding the inverse of a matrix more efficient.
## makeCacheMatrix takes a matrix and creates an object that can store it's
## inverse.  cacheSolve returns the inverse of the matrix by either solving
## for the inverse, or returning the cached value, if it exists.
###############################################################################

###############################################################################
## makeCacheMatrix
##
## This function takes a matrix as input and returns an "object"  with four
## "methods":
##  - getMatrix returns the matrix this is stored in the instance
##  - setMatrix takes a matrix as a parameter and stores it in the instance
##  - getInverse returns the _cached_ inverse of the stored matrix, or NULL
##               if no inverse has been calculated
##  - setInverse takes a matrix, that is presumably the inverse of our stored
##               matrix and caches it
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
    ## Store the inverse in the inv variable.
    inv <- NULL
    
    ## Return the value of the matrix
    gm <- function() { x }
    
    ## Store a new value for our matrix
    sm <- function(m) {
        ## Store it in the scope of this function
        x <<- m
        ## Reset our cached inverse value
        inv <<- NULL
    }
    
    ## Return the cached value of the inverse
    gi <- function () { inv }
    
    ## Store the inverse of our stored matrix
    si <- function(v) { inv <<- v }
    
    ## Return our methods as a list
    list(getMatrix = gm,
         setMatrix = sm,
         getInverse = gi,
         setInverse = si)
}

###############################################################################
## cacheSolve
##
## This function accepts an instance of a "cached matrix", created via
## makeCachedMatrix, and either returns the inverse of the mattrix.
## If there is a cached matrix inverse in the passed in instance, then we 
## return that value.  Otherwise, we use R's built-in "solve" function to 
## calculate the matrix inverse, and then store it in the instance.
###############################################################################
cacheSolve <- function(x, ...) {
    ## Read the inverse from the cachedMatrix instance
    i <- x$getInverse()
    
    ## If it's not NULL then we return the value
    if (!is.null(i)) {
        message("returning cached result")
        
    } else {
        ## Otherwise, calculate the inverse and store it in the instance.
        message("calculating the matrix inverse")
        m <- x$getMatrix()
        i <- solve(m)
        x$setInverse(i)
    }
    
    ## Return the inverse
    i
}
