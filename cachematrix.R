# Matrix Inversion with caching
#
#           USAGE:
#
# 1. create an makeCacheMatrix object
# 2. create a matrix object
# 3. use $setMatrix function to assign this matrix to makeCacheMatrix object
# 4. use cacheSolve with makeCacheMatrix object as an argument
#
# You can assign new matrix each time using the $setMatrix method
#

# Function creating an object containing functions
# that enable access to data stored in cache
makeCacheMatrix <- function() {
    
    # initialize the value of 'm'
    m <- NULL
    
    # function save new matrix ('input') in cache
    setMatrix <- function(input) {
        
        # check is 'input' is a square matrix
        if (nrow(input)!=ncol(input)) {
            return(message("non-square matrix -  hasn't been saved"))   
        }
        # if square, then basic check if invertible (by determinant)
        else if (det(input)==0) {
            return(message("non-invertible matrix - hasn't been saved"))
        }
        
        # check if 'input' and cached matrix 'm' are the same
        if (!is.null(m)) {
            # check by dimensions
            if (nrow(m)==nrow(input)) {
                # check values
                if (all(input == m)) {
                    return(print("input identical as cached matrix"))
                }
            }
        }
        
        # save 'input' in cache, set value of inverse to NULL
        if (!is.null(input)) {
                m <<- input
                i <<- NULL
                print("new matrix saved")
        }
        
    } #end setMatrix
    
    
    # function returning a matrix stored in cache
    getMatrix <- function() { m }
    
    
    # function assigning an inverted matrix to variable 'i' in cache
    setInverse <- function(invMatrix) { i <<- invMatrix }
    
    
    # function returning an inverted matrix stored in cache
    getInverse <- function() { i }
    
    list(setMatrix=setMatrix,getMatrix=getMatrix,
         setInverse=setInverse,getInverse=getInverse)
}


# Function returning inverse of a matrix with usage of
# data stored in cache. Pass a makeCacheMatrix object
# instantiated before as an argument.

cacheSolve <- function(x, ...) {
    # get current cached 'i' value
    inverse <- x$getInverse()
    
    # if it's not NULL, return
    if (!is.null(inverse)) {
        print("CACHED INVERSE")
        return(inverse)
    }
    
    # else, compute new value and store it in cache
    inverse <- solve(x$getMatrix())
    x$setInverse(inverse)
    
    # return the inverse
    print("COMPUTED INVERSE")
    inverse
}