############################################################################
## Generating a code to compute the inverse a square matrix and then also ##
##            creating a function to store the inverse matrix             ##
############################################################################

## NOTE: can always use the solve() function to find inverse of matrices  ##
## but sometimes matrix inversion is a costly computation and may help to ##
## save time if you store the inverses that are calculated                ##


############################################################################
## This function defines a series of functions that will later be used    ## 
## in the casheSolve function below.                                      ##

makeCasheMatrix <- function(x = matrix()) {
  m <- NULL #defines m as an empty value 
  set <- function(y) {
    x <<- y #<<- superassignment operator(assignment in the enclosing environment)
    m <<- NULL
  }
  get <- function() x #returns the matrix 
  setinverse <- function(inverse) m<<-inverse
  getinverse <- function() m 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  #lists each function that has been defined so they may be used in the next step 
}

##############################################################################
## This function solves for the inverse of the desired matrix and stores    ##
## this inverse value. If it has already calculated the inverse beforehand  ##
## then it will return the message "getting cached data' and return the     ##
## stored value of the inverse of that particular matrix.                   ##

casheSolve <- function(x, ...) {
  m <- x$getinverse() #calculating the inverse
  if (!is.null(m)) { 
    #if there is not a null value for the inverse of this matrix (m), 
    #then the function will return the already saved inverse matrix 
    #and return the specified message
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  #if it has not already calculated an inverse for this matrix, 
  #then it does here and stores (set) the inverse
  m <- solve(data, ...) 
  x$setinverse(m)
  m #returns the value of the inverse
}

#testing it out 

A <- matrix(c(1,2,3,4),2,2)
AI <- makeCasheMatrix(A)
AI
casheSolve(AI)
casheSolve(AI)