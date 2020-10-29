#Pair of functions to save as cache, the inverse of a matrix    

makeCacheMatrix <- function(m = matrix()){
        i <- NULL
        #Set the matrix
        set <- function(matrix){
                m <<- matrix
                i <<- NULL
        }
        get <- function(){
                m
        }
        SetInv <- function(inverse){
                i <<- inverse
        }
        GetInv <- function(){
                i
        }
        list(set = set, get = get,
             SetInv = SetInv,
             GetInv = GetInv)
}


cacheSolve <- function(m, ...){
        matrix <- m$get()
        i <- solve(matrix) %*% matrix
        m$SetInv(matrix)
        solve(matrix)
}

