## These two functions provide a way to cache the inverse of a function. 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly.

## These functions assume that a matrix is always invertible


##makeCacheMatrix creates a special "matrix" which is really a list containing
##the ability to
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse of the matrix   
##     4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(matrix) {
                x <<- matrix
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function ( solvedInverse ) inverse <<- solvedInverse
        getinverse <- function () inverse
        
        #now put these four into a list
        list (set = set, 
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)       
}


## cacheSolve calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function above. It first checks to see if the inverse
## has already been calculated. If it has, it just returns that!
cacheSolve <- function(x, ...) {
        ## Return the cached matrix that is the inverse of 'x'
        ## if we as for the inverse and get back something that isn't NULL
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return (inverse)
        }
        
        # This part is of course the "else" part: if we haven't computed the inverse yet,
        # it needs to be done. I believe in some languages code after a return statement
        # is a concern, not for r. Regardless, if the inverse hasn't been solved yet,
        # we solve it here and store the result.
        data <- x$get()
        inverse <- solve (data, ...)
        x$setinverse (inverse)
        inverse
}



## Al Warren posted an easy visual test for inverses
##I've pasted them here for easy reference.  I've added
##a quick test to make sure if you make a new matrix
##you don't get the same cached value back.

# > source('~/ProgrammingAssignment2/cachematrix.R')
# > m<-matrix(c(-1,-2,1,1),2,2)
# > x<-makeCacheMatrix(m)
# > x<-makeCacheMatrix(m)
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv<-cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv<-cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > m<-matrix(c(1,2,-1,-1),2,2)
# > x<-makeCacheMatrix(m)
# > inv<-cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv<-cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv<-NULL
# > inv
# NULL
# > inv<-cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
