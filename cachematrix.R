## These two functions will calculate the inverse of a matrix and store the results
## Next time they're called, if the value has already been stored it returns the
## stored value instead of recalculating.
##
## You need to use both function in conjunction.
## for example,
##    > a <- makeCacheMatrix(myMatrix)
##    > casheSolve(a)
## will return the inverse of myMatrix. (If it hasn't alreayd been calculated
## it calculates it and stores the value. If it has been calculated it looks up
## and returns the stored value)


## makeCasheMatrix stores 4 functions. Those 4 functions set & get the value
## of the data entered (e.g., the matrix the user inputs) and sets/gets the 
## inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ##changes the matrix stored
                x <<- y
                i <<- NULL
        }
        get <- function() x ##returns matrix stored
        setinverse <- function(inverse) i <<- inverse ##stores the inverse matrix
        getinverse <- function() i ##returns the inverse matrix
        
        ## we want to store the 4 above functions so they can be called by the 
        ##      casheSolve function. Below returns a list of those 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Returns the inverse matrix by either calculating it or retrieving the already
## calculated one
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##Checks to see if the inverse has already been calculated/stored
        i <-x$getinverse()
        if(!is.null(i)){ ##If it has been cashed (you can tell it has been cashed
                         ##because the value is not null) then return it
                message("getting cahsed data")
                return(i)
        }
        ##If we get this far in the function it means the inverse hasn't already
        ##      been cashed. So we need to calculate it and save it
        
        data <- x$get()
        i <- solve(data) ##this line calculates the inverse and assigns it to i
        x$setinverse(i) ##stores the newly calculated invrese matrix
        i ## returns the newly calculated inverse matrix
}
