## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Before the inverse is calculated, set the default 
## value of inversed matrix as NULL, save the values
## that had been calculated, and finally define a function 
## get the calculated value in cache.

makeCacheMatrix <- function(x = matrix()) {
    inverse_m <-NULL
    set<-function(y){
        x<<-y
        inverse_m<-NULL
    }
    get<-function ()x
    set_inverse<-function(solve) inverse_m<<-solve
    get_inverse<-function() inverse_m
    list(set=set,get=get, set_inverse=set_inverse, 
         get_inverse=get_inverse)

}

## Write a short comment describing this function

## To get the saved value from the previous calculated result and 
## save it in "data" using the get_inverse function, continue to 
## calculate the parts that hasn't  been progressed by calling "solve",
## and add the newly calculated results to the previous outcome.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_m<-x$get_inverse()
    if(!is.null(inverse_m)) {
        message("getting cached data")
        return(inverse_m)
    }
    data <- x$get_inverse()
    get_inverse <- solve(data, ...)
    x$set_inverse(inverse_m)
    inverse_m
}


