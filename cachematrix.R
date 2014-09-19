# Computing matrix inverse may be very expensive in term of time and space.
# Here we implement two functions which allows to store a matrix inverse once computed


# This function returns a list of function acting on

makecachematrix <- function( X = matrix() ) {
    Z <- NULL
    set <- function( Y ) {
        X <<- Y
        Z <<- NULL
    }
    get <- function() X
    setinv <- function( inv ) Z <<- inv
    getinv <- function() Z
    list( set = set, get = get,
          setinv = setinv,
          getinv = getinv )
}


# This function retrieves a chached copy of the inverse matrix of X
# X must be an output of the makecachematrix function

cachesolve <- function( X, ... ) {
    Z <- X$getinv()
    if( !is.null( Z ) ) {
        message( "getting cached data" )
        return( Z )
    }
    data <- X$get()
    message( "computing the inverse" )
    Z <- solve( data, ... )
    X$setinv( Z )
    Z
}