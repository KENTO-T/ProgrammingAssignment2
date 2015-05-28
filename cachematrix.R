makeCacheMatrix <- function(x = matrix()) { #substitute matrix x for the function
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, # make the outputs the list, so we use them available  
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) { #... through x another functions
        m <- x$getinverse()
        if(!is.null(m)) { #when mean is cached 
                message("getting cached data") #this message print
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##assignment3
best <- function(state, outcome) {
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
        HN <- as.character(outcome[,2])
        state <- as.numeric(outcome[,7])
        HA <- as.numeric(outcome[,11])
        HF <- as.numeric(outcome[,17])
        Pn <- as.numeric(outcome[,23])
        outcome2 <- data.frame(HN,state,HA,HF,Pn)
        names(outcome2)[3]<-"heart attack"
        names(outcome2)[4]<-"heart failure"
        names(outcome2)[5]<-"pneumonia"
        q1<-complete.cases(outcome2)
        q2<-outcome2[q1,]
        w1<-split(q2,q2[,2])
        w2<-w1$"state"
        w3<-min(w2[,"outcome"])
        w4<-subset(w2, w2$"outcome"==w3)
        w5<-as.numeric(w4$"HN")
        w5
        }


est <- function(state, autcome) {
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #第二引数があることでNAに持ち込めるj
        out1<-outcome[,c(2,7,11,17,23)]
        names(out1)[3:5]<-c("heart attack","heart failure","pneumonia")
        out1$"heart attack"<-as.numeric(out1$"heart attack")
        out1$"heart failure"<-as.numeric(out1$"heart failure")
        out1$"pneumonia"<-as.numeric(out1$"pneumonia")
        out2<-complete.cases(out1)
        out3<-out1[out2,]
        f<-split(out3,out3$"SC")
        f1<-f$state
        f2<-subset(f1,f1$"heart attack"==min(f1$"heart attack"))
        f2$"Hospital.Name"
}
best-3
best <- function(state, autcome) {
        # Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #第二引数があることでNAに持ち込める
        out1<-outcome[,c(2,7,11,17,23)]
        names(out1)[3:5]<-c("heart attack","heart failure","pneumonia")
        out1$"heart attack"<-as.numeric(out1$"heart attack")
        out1$"heart failure"<-as.numeric(out1$"heart failure")
        out1$"pneumonia"<-as.numeric(out1$"pneumonia")
        out2<-complete.cases(out1)
        out3<-out1[out2,]
        f<-split(out3,out3$"State") ##このStateは引数のstateではない。
        f1<-f$NN ##第一引数のstate。これから、各州を選んでくる。ここがうまく動いていない
        ff1<-if(min(f1[,5]) == "Inf"){
                stop("invalid state")
        }else{stop(min(f1[,5]))}
}

best-2
best <- function(state, autcome) {
        # Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #第二引数があることでNAに持ち込める
        out1<-outcome[,c(2,7,11,17,23)]
        names(out1)[3:5]<-c("heart attack","heart failure","pneumonia")
        out1$"heart attack"<-as.numeric(out1$"heart attack")
        out1$"heart failure"<-as.numeric(out1$"heart failure")
        out1$"pneumonia"<-as.numeric(out1$"pneumonia")
        out2<-complete.cases(out1)
        out3<-out1[out2,]
        f<-split(out3,out3$"State") ##このStateは引数のstateではない。
        f1<-f$NN ##第一引数のstate。これから、各州を選んでくる。ここがうまく動いていない
        ff1<-min(f1[,5])
        names(f1$"pneumonia")
        f2<-subset(f1,f1$"pneumonia"==ff1)
        print(f2$"Hospital.Name")
}

rankhospita-4-draft
rankhospital <- function(state, autcome,rank=i) {
        # Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #第二引数があることでNAに持ち込める
        out1<-outcome[,c(2,7,11,17,23)]
        names(out1)[3:5]<-c("heart attack","heart failure","pneumonia")
        out1$"heart attack"<-as.numeric(out1$"heart attack")
        out1$"heart failure"<-as.numeric(out1$"heart failure")
        out1$"pneumonia"<-as.numeric(out1$"pneumonia")
        out2<-complete.cases(out1)
        out3<-out1[out2,] ##欠損値を除いた表
        ss1<-split(out3,out3$"State") ## このStateは引数のstateではない。第一引数で州ごとの化
        ss2<-ss1$NY ## 分割した州のみを抜き出す
        ord1<-order(ss2$"heart attack",ss2$"Hospital.Name",decreasing = FALSE)
        ord2<-ss2[ord1,] ##　任意のRateを昇順にした表
        ran<-ord2[rank,1]
        aaa<-if( ran == "NA"){
                stop("invalid number")
        }else{ print(ran) }
        
}
rankall
rankall <- function(autcome,num=i) {
        # Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #第二引数があることでNAに持ち込める
        out1<-outcome[,c(2,7,11,17,23)]
        names(out1)[1:5]<-c("hospital","state","heart attack","heart failure","pneumonia")
        out1$"heart attack"<-as.numeric(out1$"heart attack")
        out1$"heart failure"<-as.numeric(out1$"heart failure")
        out1$"pneumonia"<-as.numeric(out1$"pneumonia")
        out2<-complete.cases(out1)
        out3<-out1[out2,] ##欠損値を除いた表
        ord1<-order(out3$"heart attack",out3$"hospitl",decreasing = FALSE)
        ord2<-out3[ord1,] ##　任意のRateを昇順にした表
        ran<-ord2[1:num,1:2]
        ran
        
}   
rankall.draft
rankall <- function(autcome,num = i) {
        # Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = F , na.strings = "Not Available") #第二引数があることでNAに持ち込める
        outcome$"heart attack"<-as.numeric(outcome$"heart attack")
        outcome$"heart failure"<-as.numeric(outcome$"heart failure")
        outcome$"pneumonia"<-as.numeric(outcome$"pneumonia")
        outcome$"Hospital.Name"<-as.character(outcome$"Hospital.Name")
        outcome$"State"<-as.character(outcome$"State")
        out1<-outcome[,c(2,7,11,17,23)]
        names(out1)[1:5]<-c("hospital","state","heart attack","heart failure","pneumonia")
}

assignment 2
## Creates a list of functions that set and get the value of a matrix along with the inverse of that matrix.   Called before cacheSolve().
## $set is passed the matrix, defines it for future function calls
## $get returns the matrix
## $setInverse is passed the inverse and defines it for future function calls
## $getInverse returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
        
    }
    get <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix previouisly defined with the makeCacheMatrix() function and prevents duplicate inverse calculations.
## Pass the list of functions created with makeCacheMatrix(x) where x is the matrix to calculate its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' where 'x' was previously passed to makeCacheMatrix.
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    invMatrix
}
