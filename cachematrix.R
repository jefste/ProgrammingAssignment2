## These functions calculate the inverse of the matrix passed in. 
## If the matrix already has the inverse computed and stored
## then the function reads out that value instead of recalculating
## the inverse of the matrix




## Function creates a vector that contains a list of functions to call
## on the matrix passed in "x"

makeCacheMatrix <- function(x = matrix()) {
    inverse.value <- NULL
    set.matrix <- function(y){
        #puts the value of y into x
        x <<- y
        #clears the value at the invese
        inverse.value <<- NULL
    }
    
    #returns the value of the matrix x
    get.matrix <- function() x
    
    #sets the inverse.value to the passed in value inverse
    set.inverse <- function(inverse) inverse.value <<- inverse
    
    #gets the inverse.value currently stored
    get.inverse <-function() inverse.value
    
    #returns list of functions
    list(set.matrix=set.matrix, get.matrix=get.matrix,
         set.inverse=set.inverse,get.inverse=get.inverse)
}


## Reads the value stored for the inverse value of the matrix
## If there is already a computed value, then the previously
## computed value is used, otherwise the inverse will be calculated
## Note the 'x' passed in must be a vector generated from
## the makeCacheMatrix. 
## example:  var.to.pass.in<-makeCacheMatrix(some.matrix))

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse.value<-x$get.inverse()
    
    ## checks to see if there is a value already calculated
    if(!is.null(inverse.value)){
        ## prints message stating that the inverse is cached
        message("getting cached inverse value")
        ## returns the cached value
        return(inverse.value)
    }
    ## reads the value of the matrix    
    data <-x$get.matrix()
    
    ## calculates the inverse of the matrix
    inverse.value<-solve(data)
    
    ## sets the inverse value. This allows us to read it from the
    ## cache next time we call cacheSolve on this matrix
    x$set.inverse(inverse.value)
    
    ## returns inverse of matrix
    inverse.value
}
