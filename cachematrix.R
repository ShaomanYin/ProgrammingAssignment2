## Programming Goal: Cache the inverse of a matrix

## Matrix inversion is usually a costly computation and there may be some 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The following is a pair of functions that cache the 
## inverse of a matrix.
## Assuming the input matrix is always invertible.

## The function "makeCacheMatrix" creates a special "matrix" object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
       ##set the value of matrix
            inv<-NULL
            set<-function(y) {
               x<<-y
              inv<<-NULL
          }
       ##get the value of matrix
       get<-function()x
       #set the value of inverse 
       setinv<-function(inverse) inv<<-inverse
       #get the value of inverse 
       getinv<-function() inv
       list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The function "cacheSolve" computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated(and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ##check if inverse has already been calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
        ## Return a matrix that is the inverse of 'x'
}


##Showing the reduced computer time using these functions
set.seed(12345)
r<-rnorm(1000000)
a<-matrix(r,1000,1000)

##conventional method (method 1)
start.time1 = Sys.time()
solve(a) 
time1 = Sys.time() - start.time1
print(time1)

##use the funcitons (method 2)
start.time2 = Sys.time()
b<-makeCacheMatrix(a)
cacheSovle(b)
time2 = Sys.time() - start.time2
print(time2)

##time2 will be slightly less than time1
##the method 2 will be more efficient during large iterative process  


###########################################################################END


