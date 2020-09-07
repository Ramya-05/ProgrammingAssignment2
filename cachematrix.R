## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
   ## that can cache its inverse.
##.  `cacheSolve`: This function computes the inverse of the special
  ##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
 ##   already been calculated (and the matrix has not changed), then
 ##   `cacheSolve` should retrieve the inverse from the cache.


library(MASS)

##makeCacheMatrix`: This function creates a special "matrix" object
  ##  that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        
inv=NULL

    set<-function(y){
        x<<-y

       inv<<-NULL
        }
            get<-function()x
              
           setinv<-function(inverse)inv<<-inverse
    getinv<-function(){
    inver<-ginv(x)
    inver%*%x
    }
 list(set=set,get=get,setinv=setinv,getinv=getinv)  


}


## cacheSolve`: This function computes the inverse of the special
   ## "matrix" returned by `makeCacheMatrix` above. If the inverse has
    ##already been calculated (and the matrix has not changed), then
  ##  `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        x$getinv()
        if(!is.null(inv)){
          message("getting cached data")
        return(inv)
        
        }
## get matrix using object
     data<-x$get()
     inv<-solve(data....)
     x$setinv(inv)
     inv
        ## Return a matrix that is the inverse of 'x'
}
f<-makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$getinv()
cacheSolve(f)
