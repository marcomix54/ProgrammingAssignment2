# this function creates a list of 4 functions: set, get, setInverse and getInverse
# we use the "<<-" assignment operator in the aim to confine the variables
# and not expose them the to outside environment. 


makeCacheMatrix <- function(x = numeric()) {
        
        result_inv_matrix <- NULL # We set up the object where the result of the inversion is stored
        set <- function(y) {
                x <<- y
                result_inv_matrix <<- NULL
        }
        
        get <- function() x # We store in get the input matrix
        
        setInverse <- function(inverse) result_inv_matrix <<- inverse # We set the inversed matrix
        getInverse <- function() result_inv_matrix # Return the inversed matrix in getInverse
        
        # Create a list that contains the functions, in this case we will use makeCacheMatrix in the following way
        # x <- makeCacheMatrix(matrix1)
        # x$set(matrix2) # For a new matrix
        # x$get # to get the setted matrix
        # x$setInverse # to set the inversed matrix
        # x$getInvverse # to get the inversed matrix
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        
        m <- x$getInverse() # get the inversed matrix from x
        
        if(!is.null(m)) { # if we already have a result in m
                message("getting cached data") # Return a message to inform
                return(m) # return the inversion
        }
        
        data <- x$get() # if not, x$get to get the matrix
        m <- solve(data) # we solve the matrix
        x$setInverse(m) # we then set it to the object
        m # return the result
}

# TEST

# Computing the inverse of a square matrix can be done with 
# the solve function in R. For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied 
# is always invertible.

test <- matrix(runif(36,1,100),6,6) # We generate a matrix with unique values
testCache <- makeCacheMatrix(test) # Generate the object with this matrix

# Now we can calcultate the inversion using the cachSolve function

testInverse <- cacheSolve(testCache)