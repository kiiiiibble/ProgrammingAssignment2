## Put comments here that give an overall description of what your
## functions do
##These functions allow the user to create a special matrix
##that caches its inverse once calculated. The inverse must be
##calculated using a special function in order for it to be cached.

## Write a short comment describing this function
##This function is for creating the matrix. It also contains
##functions within it for getting and setting the original
##matrix and its inverse. It returns a list.

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        get<-function() x
        
        setInverse<-function(inverse) inv<<-inverse
        
        getInverse<-function()inv
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
##This function gets the inverse of the defined cache matrix.
##If there is a cached inverse, this gets the cached value
##instead of calcuating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i<-x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        mat<-x$get()
        
        i<-solve(mat,...)
        
        x$setInverse(i)
        i
}
