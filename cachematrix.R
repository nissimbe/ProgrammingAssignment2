## Put comments here that give an overall description of what your
## functions do
#
# cacheSolve calcs the inverse of matrix if  not in cache, otherwise return cached value
# input: a list object which is created  by "makeCacheMatrix" function containing
#
#    Operation transcript:
#
# 1) ---  makeCacheMatrix   ---  
# > resMat<-makeCacheMatrix(matrix(2:5,2,2))
# > resMat$getMat()
# [,1] [,2]
# [1,]    2    4
# [2,]    3    5
# > 
# > resMatEmpty<-makeCacheMatrix()
# > resMatEmpty$setMat(matrix(4:7,2,2))
# > resMatEmpty$getMat()
# [,1] [,2]
# [1,]    4    6
# [2,]    5    7
# > 
#
# 2) ---    cacheSolve  ---  
# > resMat<-makeCacheMatrix(matrix(2:5,2,2))
# > resMat$getMat()
# [,1] [,2]
# [1,]    2    4
# [2,]    3    5
# > cacheSolve(resMat)
# [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1
# > cacheSolve(resMat)
# getting cached data
# [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1
# >
# 


# ------------------------------------------------
#   initialize values for invMat (cache) and myMat (input matrix) 
#   return 4 functions to set/get the above 2 values
#   See examples of operation transcript above
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL             
  
  setMat <- function(y) {    #set/change matrix value from input like:  objName$setMat(matrix(1:4,2,2) )
    myMat <<- y              # effect of <<- assignment is like a global variable
    invMat <<- NULL          #  .. so it is suitable for use as a cache
  }
  
  getMat <- function() myMat #gets matrix like: objName$getMat()
  
  setInvMat <- function(inv){ 
    invMat <<- inv  
  }
  
  getInvMat <- function() invMat
  list(setMat=setMat, getMat=getMat,setInvMat=setInvMat , getInvMat=getInvMat)
}



# ------------------------------------------------
#   cacheSolve calcs the inverse of matrix if  not in cache, otherwise return cached value
#   input: a list object which is created  by "makeCacheMatrix" function containing
#   See examples of operation transcript above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInvMat()  #fetch cached(or NULL) value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMat()
  inverse <- solve(data, ...)
  x$setInvMat(inverse)    # set value in cache
  inverse                 # return value
}
