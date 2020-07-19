#Introduction
#This second programming assignment will require you to write an R function that is able to cache potentially time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. In this Programming Assignment you will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.

#Example: Caching the Mean of a Vector
#In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and caches its mean.

#The first function, makeVector creates a special "vector", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
#The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
#Assignment: Caching the Inverse of a Matrix
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:
  
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.

#In order to complete this assignment, you must do the following:
#Fork the GitHub repository containing the stub R files at https://github.com/rdpeng/ProgrammingAssignment2 to create a copy under your own account.
#Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.
#Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).
#Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.
#Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.
#Grading
#This assignment will be graded via peer assessment.

###################################Assignment Week 3###############################
#For this assignment we will be using the matlib library that has a function
#to get a matrix's inverse called inv(), first of all I focused on the vector
#examples that were given at the beginning of the assignment, I first changed 
#in the function makeVector the x, in the beginning it was numeric to create
#vectors, but I needed a matrix instead, after that, the code is basically
#the same as the vector, what changes is the inv() function that will give
#us the inverse of the matrix that we created, this functions focus on the article 
#Demistifying makeVector(), which was written by Len Greski´s, in here x is a 
#matrix and it is part of the function´s argument and m starts as a NULL values that
#will be used for further operations, then we use the operator <<- that is used 
#to assign the value on the right side to an object in the parent environment and
#avoids looking for the functions in the global environment and we then use the get function
#to get it from the parent environment, after this we define where we will set
#the inverse of the matrix and at the end an object is created by returning a list
#to the parent environment, after makeCacheMatrix is done, we have to create
#cacheSolve, another function that will retrieve information from makeCacheMatrix
#and will do the operation of the matrix´s inverse, here we will retrive information
#with the $ command and we put the condition if, and we want to know if the result is NULL
#if a matrix is set and if it is not NULL we have a valid cached inverse, otherwise if the result
#in !is.null(m) is false, we need to retrive the data from the matrix and get the inverse with
#the function inv(). In cacheSolve is where the inverse is executed, makeCacheMatrix is
#incomplete without cacheSolve, such as in teh example makeVector, both functions 
#complement each other to give us the result we need.

install.packages(matlib)
library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data,...)
  x$setinverse(m)
  m
}

mat1 <- matrix(c(4,3,7,6,8,10,23,52,64), ncol = 3, nrow = 3)
t <- makeCacheMatrix(mat1)
cacheSolve(t)

#Results from previous example
#          [,1]        [,2]        [,3]
#[1,] -0.01990050 -0.38308458  0.31840796
#[2,]  0.42786070  0.23631841 -0.34577114
#[3,] -0.06467662  0.00497512  0.03482587

