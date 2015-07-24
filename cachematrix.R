## makeCacheMatrix function creates a matrix object that can be used cache its inverse.It however does not calculate the inverse
## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function. 
##If the inverse has already been calculated and also the matrix has not been changed, 
##then the cachesolve retrieves the inverse from the cache and prints it out

## makeCacheMatrix is a function that stores a list of functions
##makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inver<- NULL
        ##set is a function that changes the matrix stored in the main function (makeCacheMatrix).
        set <- function(y = matrix()){
                x <<- y
                inver<<- NULL 
        }
     ## get is a function that returns the matrix x stored in the main function(makeCacheMatrix). Doesn't require any input.  
        get <- function() x
     
     ##setinverse and getinverse don't calculate the inverse of the matrix, 
     ##they simply store the value of the input in a variable inver into the main function makeCacheMatrix (setinverse) and return it (getinverse)
        setinverse <- function(solve = matrix()) inver <<- solve 
        getinverse <- function() inver
     ##To store the 4 functions in the function makeCacheMatrix, we need the function list(),
     ##so that when we assign makeCacheMatrix to an object, the object has all the 4 functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The first thing cacheSolve does is to verify the value inver, stored previously with getinverse, exists and is not NULL.
##If it exists in memory, it simply returns a message and the value inver, that is supposed to be the inverse of the matrix, but not necessarily.

     cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##If the inverse of the matrix is in the memory then, "return(m)" would have ended the function
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##data gets the matrix stored with makeCacheMatrix, m calculates the inverse of the matrix
        ##x$setinverse(m) stores it in the object generated assigned with makeCacheMatrix.
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
     
        
}
############## Sample Output###########################


##> mat <- matrix(c(0,0,2,55,23,54,8,10,25,65,15,44,19,30,100,120),nrow = 4,ncol = 4)
##solve(mat)
##[,1]         [,2]         [,3]          [,4]
##[1,]  0.16332107 -0.068137639 -0.033886580  0.0194140575
##[2,]  0.39645092 -0.145663698 -0.033069603  0.0012025310
##[3,] -0.33651033  0.140088665  0.022910075 -0.0008330937
##[4,]  0.01549405 -0.007997451  0.009886789 -0.0003595196

##matrixx <- makeCacheMatrix(mat)
##matrixx$get()
##[,1] [,2] [,3] [,4]
##[1,]    0   23   25   19
##[2,]    0   54   65   30
##[3,]    2    8   15  100
##[4,]   55   10   44  120

##> matrixx$getinverse()
##NULL

##> cacheSolve(matrixx)
##[,1]         [,2]         [,3]          [,4]
##[1,]  0.16332107 -0.068137639 -0.033886580  0.0194140575
##[2,]  0.39645092 -0.145663698 -0.033069603  0.0012025310
##[3,] -0.33651033  0.140088665  0.022910075 -0.0008330937
##[4,]  0.01549405 -0.007997451  0.009886789 -0.0003595196


##> cacheSolve(matrixx)
##getting cached data
##[,1]         [,2]         [,3]          [,4]
##[1,]  0.16332107 -0.068137639 -0.033886580  0.0194140575
##[2,]  0.39645092 -0.145663698 -0.033069603  0.0012025310
##[3,] -0.33651033  0.140088665  0.022910075 -0.0008330937
##[4,]  0.01549405 -0.007997451  0.009886789 -0.0003595196
