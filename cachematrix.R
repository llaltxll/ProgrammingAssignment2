## this file contains a constructor function for cachable inverse matrix object
## and a function to solve, set and if avalible get an invers matrix using an
##instance of the object above

## this is a cunstructor function to create an invers matrix cash for a givven
## matrix and store it in an object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## this is a function that checks if the instance of the object above already has
## an invers matrix cashed, if it does then the function skips solving and returns 
## the existing cashed result from within the object.
## if the object is yest to be solved than it goes on to solve for the object's 
## matrix, stores the result in the object for later use (cash) and returns the
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
