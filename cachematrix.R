makeCacheMatrix <- function(x = matrix()) 
{
        ## This function stores a list of four functions that 
        ## can cache (set) and retreive (get) the matrix and its inverse
        ## as follows:
        ## (1) set the square matrix entries (or it elements).
        ## (2) get the square matrix entries (or it elements).
        ## (3) set the value of the matrix inverse.
        ## (4) get the value of the matrix inverse
        
        
        ## "set" is a function that changes the matrix stored in the main function.
        ## We don't need to use the function unless we want to change the matrix.
        ## x <<- y subsitutes the matrix x with y (the input) in the main
        ## function (cachematrix.R)  If a "new" matrix is input, the "old" stored
        ## inverse (im) is set to null as "new" matrix inverse need to be calculated
        ## using the cachematrix.R function.
        
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ## "get" is a function that returns the matrix x stored in the main
        ## function. It does not require any input.
        
        get <- function() x
        
        ## "setinverse" and "getinverse" are functions very similiar to set and get.
        #setinverse <- function(inverse) im <<- inverse
        
        setinverse <- function(solve) im <<- solve
        
        getinverse <- function() im
        
        
        ## The following creates the list of the 4 functions.    
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
        ## This function calculates and returns a matrix that is the inverse of a
        ## square matrix. However, it first checks to see if the matrx inverse has
        ## already been calculated. If so, it gets the inverse from the cache and
        ## skips the computation: otherwise, it calculates the inverse of the matrix
        ## using the solve generic function.
        
        im <- x$getinverse()
        if(!is.null(im)) 
        {
                message("getting cached data")
                return(im) 
        }
        
        ##Solve calculates the inverse matrix(im)
        data <- x$get() 
        im <- solve(data,...) 
        
        ## The setinverse places the inverse matrixin the cache
        
        x$setinverse(im)
        
        ##return(im)
        im
}

