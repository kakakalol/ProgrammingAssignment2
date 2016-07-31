##these two functions work together to cache the inverse of a matrix

##the first one creates a matrix object that is able to cache its inverse, yet unable to do the computation of inversion

makeCasheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(minv)m<<-minv
  getinv<-function()m
  list(set=set,get=get,setinv=setinv,
       getinv=getinv)
}
##this function can compute the inverse of the matrix 
##returned by the first function, and if the inverse has
##already been calculated,it can retrieve it from the cache
##
cacheSolve<-function(x,...){
  m<-x$getinv
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}