#How to call and test the function #approach #1
#source("makeMatrix.R")
#a<-(matrix(1:4,2,2))
#b<-makeCacheMatrix(a)
#cacheSolve(b)

#How to call and test the function #approach #2
#source("makeMatrix.R")
#mat <- matrix(data = c(3,-7,5,2), nrow = 2, ncol = 2)
#mat2 <- makeCacheMatrix(mat)
#cacheSolve(mat2)

"Inverse of a A matrix e.g
A= |3   5|
   |-7 2|
A`=1/det(A) * adj(A)= 1/(3*2- (-7)*5 )* |2   -5|
                                        |7    3|
					= 1/41 |2 -5| = |2/41   -5/41|
					       |7  3|   |3/41    3/41|
						
			        = |.04 -.12|
				      |.17  .07|"
makeCacheMatrix <- function(x = matrix()) {
  x_inv<-NULL
  setMatrix<-function(y){
  x<<-y
  x_inv<<-NULL
}
#get the matrix from the list and return it
getMatrix<-function() x
#inversed matrix is passed from cacheSolve method by a function call
setInvMatrix<-function(inverse) {x_inv<<- inverse}
#inversed matrix is returned to cacheSolve method by a function call
getInvMatrix<-function() x_inv
list(setMatrix=setMatrix, getMatrix=getMatrix,
   setInvMatrix=setInvMatrix,
   getInvMatrix=getInvMatrix)
}

cacheSolve <- function(x=matrix(), ...) {
	#retrieves the stored matrix by calling the function defined in makeCacheMatrix 
    x_inv<-x$getInvMatrix()
	#check whether the incoming matrix is retrieved by the getInvMatrix function  call
	#check 	whether the retrieved matrix is NULL or not
    if(!is.null(x_inv)){
      message("getting cached data")
      return(x_inv)
    }
	#matrix is not stored as Inverse in the cache list
	#so retrieve it from the list
    matrix<-x$getMatrix()
	#use generic R function solve() to find the inverse of a matrix
    x_inv<-solve(matrix, ...)
	# setMatrix inversed matrix on to incoming argument
    x$setInvMatrix(x_inv) 
    x_inv
}
