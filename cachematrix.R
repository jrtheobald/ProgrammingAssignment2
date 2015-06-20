## These functions -- makeCacheMatrix and cacheSolve -- will calculate the inverse
## of a matrix and store its value for recall.

## makeCachematrix creates a matrix in which data will be input 
## for calculating the inverse
## makeCachematrix has only one argument and is required to be a matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      ## m is a variable assigned NULL to replace the data entered.
      ## This occurs when m is called.
        m <- NULL
        
      ## Here makeCacheMatrix is assigned the value of the 'x' input by the user
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
      ## makeCacheMatrix has to retrieve 'x' assigned by 'set'.
      ## Either that is 'NULL' or it is a matrix.
        get <- function() x
        
      ## setsolve assigns a value that has been calculated that will be recalled.
      ## getsolve recalls the value assigned by setsolve.
      ## In this case the value is the output of the matrix 'x' passed through
      ## the solve function.  'm' is called here so the 'solve' argument is replaced
      ## with NULL once the function is done.
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
      ## This list stores all the functions so that the values are saved for recall.
        list(set = set, get = get, 
                setsolve = setsolve, 
                getsolve = getsolve)

}


## cacheSolve passes the data in the matrix provided in makeCacheMatrix through
## the solve function and stores the result in cache.

## cacheSolve takes multiple arguments including the 'x' matrix entered in makeCacheMatrix.
## search R help files for ?solve for all arguments.

cacheSolve <- function(x, ...) {
      ## The variable 'm' gets the value assigned to 'getsolve' givent the 'x' matrix
      ## If m is not NULL -- meaning a matrix was passed into makeCacheMatrix --
      ## m gets the matrix values which are returned.  This only checks for data.
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
      ## If a matrix has been input, the value of 'get' from makeCacheMatrix is assigned to data.
      ## The variable 'm' is assigned the solve function passing through data from 'get'
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
      
      ## Return a matrix that is the inverse of 'x'
        m
}
