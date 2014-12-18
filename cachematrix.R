## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    ## define function makeCacheMatrix
    invers_x <- NULL                           ## define value as a null object
    set <- function(y) {                       ## define function set
        x <<- y                                ## reset a new matrix
        invers_x <<- NULL                      ## reset inverse_x to NULL
    }
    get <- function() x                             ## returns the matrix x
    set_inv <- function(solve) inverse_x <<- solve  ## sets the solve, inverse_x, to solve
    get_inv <- function() inverse_x                 ## returns the inverse_x, inverse
    
    list(set = set, get = get,                      ## returns the 'special vector' containing all
    set_inv = set_inv,                              ## of the function just defined
    get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                     ## define function cacheSolve
    
        ## Return a matrix that is the inverse of 'x'
        
        invers_x <- x$get_inv()                      ## define variable for inverse matrix
        
        if(!is.null(invers_x)) {                     ## check a cache
            
            message("getting cached inverse matrix") ## if we have inverse matrix in cache then
            return(invers_x)                         ## return it with message
        }
        else {                                       ## else
            data <- x$get()                          ##
            invers_x <- solve(data, ...)             ## compute it
            x$set_inv (invers_x)                     ##
            return (invers_x)                        ## and return it 
        }
}
