## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. 
## cacheSolve retrieves the inverse from cache, if not changed.

## Creating special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
cacheInverse <- NULL
set <- function(y = matrix()){
x<<-y
cacheInverse <<- NULL
}

get <- function() x

setInverse <- function(inverseVal) {
cacheInverse <<- inverseVal
return(cacheInverse)
}

getInverse <- function() cacheInverse
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Check if there exists a cached inverse value, if not, compute and return it 

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2, ...) {
        ## Return a matrix that is the inverse of 'x'
calInverse <- x$getInverse()

if(!isnull(calInverse) && is.matrix(calInverse)) {
message("getting cached data")
return(calInverse)
}

matrixS <- x$get()

calInverse <- tryCatch({
solve(matrixS)
}, warning=function(w) {
message("This result looks different and not the original")
message(w)
}, error=function(e) {
message("Noticed error while solving the matrix")
message(e)
})


message("Set the inverse value to:")
x$setInverse(calInverse)
}




