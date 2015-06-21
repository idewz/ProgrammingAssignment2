# Create special object that can cache the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inversed_matrix <- NULL

    set <- function(y) {
        x <<- y
        inversed_matrix <<- NULL
    }
    get <- function() x

    setInversed <- function(x) inversed_matrix <<- x
    getInversed <- function()  inversed_matrix

    list(set = set,
         get = get,
         setInversed = setInversed,
         getInversed = getInversed
    )
}

# Computes the inverse of CacheMatrix 'x'
cacheSolve <- function(x) {
    inversed_matrix <- x$getInversed()

    # returns cache if is has been already calculated
    if ( !is.null(inversed_matrix) ) {
        message("getting cached data")
        return(inversed_matrix)
    }

    # if not cached, calculate the new one, then cache it
    data <- x$get()
    inversed_matrix <- solve(data)
    x$setInversed(inversed_matrix)

    inversed_matrix
}
