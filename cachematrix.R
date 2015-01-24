## Calculating and caching the value of an inverse of a matrix

## calculates a cached value of the inverse of the matrix in the parent environment
## and creates a list with the caching and retrieval funtions

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse <-function(solve) m<<-solve
        getinverse<- function() m
        list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}


## checks whether cached value of an inverse is available. Retrieves cached value if
## it is available or calculated and caches it if not

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
