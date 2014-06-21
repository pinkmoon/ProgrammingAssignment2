## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing << makeCacheMatrix >>
## this function takes an invertible matrix vector as an input parameter (i.e., 'x') and creates a list object that
## contains 4 accessor functions allowing you to get/set the value of the matrix and get/set the value of
## the inverse operation on the matrix

## the inverted matrix is stored in an object variable 'm', whose scope is within the makeCacheMatrix itself
## the original matrix data is stored in a separate internal object variable 'x'

## the 'set' function clears the inverted matrix value 'm' (to NULL) and allows a new matrix value to be
## stored internally within the makeCacheMatrix function
## the 'get' function returns the original (unprocessed) matrix data

## the 'setInverse' function assigns the inverted matrix data to the storage object 'm'
## the 'getInverse' function returns the inverted matrix data object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) m <<- inverse
        
        getInverse <- function() m
        
        ## return list object with 4 accessor functions that operate on 'x' and 'm' stored data objects
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing << cacheSolve >>
## this function takes an input parameter matrix object and checks to see if an inverted matrix value from a previous
## inverse operation is already being held, in which case this value is returned on function exit

## if there has been no matrix inverse operation on the supplied matrix data 'x', then this data is
## passed to the solve() function to calculated the inverted matrix value, which is stored in 'm'

## if a cached inverted matrix data value is already present, then the inversion operation is not 
## repeated, and the cached data is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data) ## perform the matrix inversion calculation
        
        x$setInverse(m) ## cache the inverted matrix value
        
        m ## return the inverted matrix value 
}
