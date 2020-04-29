## Put comments here that give an overall description of what your
 ## functions do
 ##Caching inverse of matrix: this function is caching the inverse of the matrix,
 ## inversion of the matrix is a costly affair & chaching the results to display the same later can be retrieved

 ## Write a short comment describing this function
 ## makeCacheMatrix: this function creates a matrix object which can cache its inverse

 makeCacheMatrix <- function(x = matrix()) {

   inv<-NULL
   set<-function(y){
     x<<-y
     inv<<-NULL

   }
   get<-function() x
   setinverse<-function(inverse) inv<<-inverse
   getinverse<-function() inv
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

 }


 ## Write a short comment describing this function
 ## cacheSolve: this function calculates the inverse of the matrix created by makeCacheMatrix.
 ## if the value is previously calculated it prints the value from the cache memory
 #if the inputed matrix is new then it recalculates the matrix

 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){

    message("getting chached data")
    return(inv)

  }

  Mat<-x$get()
  inv<-solve(Mat, ...)
  x$setinverse(inv)
  inv

 }
 a<-matrix(data = 1:4,nrow=2,ncol=2)
 b<-makeCacheMatrix(a)
 cacheSolve(b)
