## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        invmat = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                invmat <<- NULL
        }
        get = function() x
        setinverse = function(inverse) invmat <<- inverse 
        getinverse = function() invmat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        
        invmat = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(invmat)){
                # get the inverse matrix from the cache. 
                message("getting cached data")
                # return the cached inverse matrix.
                return(invmat)
        }
        
        # if the inverse matrix is not found then calculate the inverse 
        mat.data = x$get()
        invmat = solve(mat.data, ...)
        
        # set the value of the inverse matrix in the cache via the setinverse function.
        x$setinverse(invmat)
        
        return(invmat)
}
