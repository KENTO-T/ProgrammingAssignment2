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
