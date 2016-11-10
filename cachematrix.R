## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function makeCacheMatrix creates a list containing a function to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix

makeCacheMatrix<-function(x=matrix()) {
        m<-NULL
             set<-function(y) {
                         x<<-y
                         m<<-NULL
                     }
             get<-function() x
             setinv<-function(inverse) m<<-inverse
             getinv<-function() m
             list(set=set, get=get,
                             setinv= setinv,
                             getinv= getinv)
}


## Write a short comment describing this function

## The following function calculate the mean of the inverse of the matrix, after first
## checking if the inverse has already been calculated. If so, it returns the inverse 
## and skip the computation. If not, it computes the inverse and sets the value through
## the setinverse function.
cacheSolve<-function(x,...) {
        m<-x$getinv()
           if(!is.null(m)) {
                         message("getting cached data")
                         return(m)
                    }
             matrix.data<-x$get()
             m<-solve(matrix.data, ...)
             x$setinv(m)
             m
## Return a matrix that is the inverse of 'x'
}
