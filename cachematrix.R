##it sets and stores, the data and Inverse data of a given matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


##checks if the inverse data of a matrix is stored in the cahce
##returns cached data with the return(), which terminates the function
## if i(inverse data) is null then it calculates, and returns the inverse data
cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {       
                message("getting cached data")
                return(i)       
        }
        data <- x$get()         
        i <- solve(data, ...)
        x$setInv(i)
        i
}
