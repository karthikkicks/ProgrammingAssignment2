## These functions will create a cached matrix and save the inverse and return from cache if already calculated

## This function will get a matrix vector and save it in cache. 
##Also it will give the inverse from the cache if it's already calculated

makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    set<- function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) i<<-inverse
    getInverse<-function() i
    
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)

}


## This function will calculate and give inverse. If already calculated, will get it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)){
        
        message("getting cached data")
        return(i)
        
    }
    data<-x$get()
    i<-solve(data)
    x$setInverse(i)
    i
}
