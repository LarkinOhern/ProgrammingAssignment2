## Put comments here that give an overall description of what your
## functions do
##These functions save computational resources by cacheing the values of a matrix
##and then refering to that chache to find its inverse instead of solving everytime   

## Write a short comment describing this function
##This function creates a special vector to cache values of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function returns a matrix that is the inverse of 'x' using
##the cached value if possible

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m
}
