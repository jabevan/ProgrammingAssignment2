## The following functions create a square matrix, and calculate, and cache its inverse.
## The functions include prompts to let the user know whether the inverse has been cached,
## or whether the inverse is being calculated.  It also reports the execution time 
## to illustrate the benefit in caching versus calcuating.

## NOTE: I purposely did not print the results to the screen.  This excercise is meant
## to illustrate the speed of caching versus calculating, and very large matrices are the
## best means to illustrate that difference.  It is not feasible to print matrices with
## more than a million entries to the screen.

## To test the functions in this script, please enter the following:
## > source("cachematrix.R")
## > a <- makeCacheMatrix(matrix(rnorm(1:1000000), 1000, 1000))
## > cacheSolve(a)

## At this point you will see the prompt that the inverse of the matrix is being calculated,
## and will see how much time it takes to calculate.  Entere the following again:

## > cacheSolve(a)

## You will see the prompt that the cache is being retrieved, and how long it takes.

## To really illustrate the value of caching calculations, rerun the above using:
## > a <- makeCacheMatrix(matrix(rnorm(1:4000000), 2000, 2000))



## The makeCacheMatrix function sets/gets the value of the entered matrix,
## and sets/gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL     # m is the variable for the cache, originally NULL.
        
        set <- function(y) {     # saves the entered matrix.
                x <<- y     # the <<- superassignment operator saves the variable to the global environment, for use in the function below.
                m <<- NULL
        }
        
        get <- function() x     # retrieves the entered matrix.
        
        setcache <- function(solve) m <<- solve     # saves the inverted matrix to m, the cache.
        
        getcache <- function() m     # retrives the cached inversion.
        
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
        
}


## The cacheSolve function checks whether the inverse of a matrix has been saved.
## If not, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {

        m <- x$getcache()     # retrieves the cached inversion.
        
        if(!is.null(m)) {     # checks to see if the cache has a value, or is empty.
                start <- Sys.time()  #  starts the time clock.
                message("Getting cached data...")     # notifies user a cache exists.
                m     # retrieves cache, NOTE: does not print matrix (see above).
                end <- Sys.time()     # stops the time clock.
                message("Execution time is ", end - start, " seconds.")  # reports execution time of retrieving cache.
        }
        else {     # if the cache is empty (NULL), calculates the inverse of the matrix.
                start <- Sys.time()     # starts the time clock.
                message("Calculating inverse of matrix...")     # notifies user the inverse is being calculated.
                data <- x$get()     # retrieves the entered matrix.
                m <- solve(data, ...)     # calculates the inverste of the matrix.
                x$setcache(m)    # saves the calculation to the cache (m).
                m     # retrieves the cache, NOTE: does not print the matrix (see above).
                end <- Sys.time()     # stops the time clock.
                message("Execution time is ", end - start, " seconds.")     # reports execustion time of calculating the invere of the matrix.
        }
}
