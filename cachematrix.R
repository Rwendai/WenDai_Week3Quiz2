#Assignment: Catching the inverse of a Matrix

#Matrix inversion is usually a costly compytation and there may be some benefit to canching the inverse of a matrix rather than computing it repeatedly.

#Below are a pair of functions that cache the inverse of a matrix.
#makeCacheMatrix is a function that create a special "matrix" object that can cache its inverse. 

source("/Users/wenzhang/Desktop/coursera/R/ProgrammingAssignment2-master/cachematrix.R")

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

matrix1<-matrix(1:4,2,2)
matrix1
     [,1] [,2]
[1,]    1    3
[2,]    2    4

Thismatrix<-makeCacheMatrix(matrix1)
Thismatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4

Thismatrix$getInverse()
NULL

#cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

cacheSolve(Thismatrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

Thismatrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

solve(matrix1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
