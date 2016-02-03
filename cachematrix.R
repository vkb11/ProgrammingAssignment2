## Using cache to to reduce system resources and cost for repeated task
## We'll write a pair of functions for one such task called the "Matrix Inversion"

## Function 1: "makeCacheMatrix" 

makeCacheMatrix <- function(x = matrix()) {m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## This function creates a special "matrix" object that can cache its inverse.

## Function 2: "cacheSolve" Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x= matrix(), ...) {m<-x$getmatrix()
if(!is.null(m)){return(m)}
matrix<-x$get()
m<-solve(matrix, ...)
x$setmatrix(m)
m
}

##This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above.
##If the inverse has already been calculated (and the matrix has not changed), then "cachesolve", 
##will retrieve the inverse from the cache

## Testing the "Inverse Matrix" function
## Step 1: Assign (set) a matrix to "makeCacheMatrix"
## Step 2: Run "cacheSolve" on "makeCacheMatrix" and it will return the inverse of the set matrix.

q<- makeCacheMatrix()
m<- matrix(c(2,4,1,3,2,9,8,1,3), 3, 3)
q$set(m)
cacheSolve(q)

  