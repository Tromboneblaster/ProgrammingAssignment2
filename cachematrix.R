##This function creates a special "matrix" object that can cache its inverse.

##Respectively sets and gets matrix and inverse

makeCacheMatrix<-function(x=matrix()){     
        inv=NULL
        set=function(y){
                x<<-y
                inv<<-NULL
        }
        get=function()x
        setinv=function(inverse)inv<<-inverse 
        getinv=function()inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse of the original matrix input to makeCacheMatrix

cacheSolve<-function(x,...){
        inv=x$getinv()
        if(!is.null(inv)){
                return(inv)
        }
        mat.data=x$get()
        inv=solve(mat.data,...)
        x$setinv(inv)
        return(inv)
}