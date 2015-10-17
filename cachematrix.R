## build a constructor function a new object which takes a matrix as input caches the inverse of the matrix
## build a function to retrieve the cached inverse matrix or calculate one

## constructor function a new object which takes a matrix as input caches the inverse of the matrix

makeCacheMatrix <- function(x=matrix()){    
    iv <- NULL
    set <- function(y){
        y <<- x
        iv <<- NULL
    }
    get <- function() x    
    setiv <- function(inversem) iv <<- inversem    
    getiv <- function() iv    
    list(set=set, get=get, setiv=setiv, getiv=getiv)
}

## retrieve cached inverse matrix or calculate and set the inverse matrix if not yet available

cacheSolve <- function(x, ...){
    data <- x$get()
    iv <- x$getiv()
    if(!is.null(iv)){
        message("getting inverse matrix")
        return(iv)
    }
    
    iv <- solve(data, ...)
    x$setiv(iv)
    iv
    
}
