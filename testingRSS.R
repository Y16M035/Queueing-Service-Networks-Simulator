source("queues.R")

a<-list(list(0,0,0,0,0,0),list(0,0,0,0,0,0),list(0,0,0,0,0,0),list(0,0,0,0,0,0),list(0,0,0,0,0,0),list(0,0,0,0,0,0))

#returns a matrix where each row corresponds to the element
#and each column to the position it has exited at
#the element is the number of times that has happened
for(k in 1:10000000){
  b<-queue("RSS")
  b<-push(b,1)
  b<-push(b,2)
  b<-push(b,3)
  b<-push(b,4)
  b<-push(b,5)
  b<-push(b,6)
  for(i in 1:6){
    POP<-pop(b)
    b<-POP[[1]]
    j<-POP[[2]]
    a[[i]][[j]]<-a[[i]][[j]]+1
  }
}
