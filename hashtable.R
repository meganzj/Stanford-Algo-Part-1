#Very Useful package, if you want to find what package does a function belongs, use finFn() in 'sos'
install.packages("sos")
library("sos")
findFn("laply")   

my.matrix<-matrix(0,1000,2)
my.list<<- rep(list(my.matrix),20001)
####################################################################################################
g = read.table("hash2sum.txt")
g=data.frame(sort(g[,1]))
#x<-as.vector(g[,1]) #convert dataframe to vector
colnames(g)[1]="x"
g$n<-floor(log10(abs(g[,1])) + 1) - 1 #scientific notation: a*10^n, n store 'n'
g$a<-g[,1]/(10^(g[,2])) #Scientific notation: a * 10^n, 'a' stores 'a'
g$floor<-as.integer(floor(g[,3]))   #g[,3] is column 'a', floor calculate floor(a) 
g$ceil<-as.integer(ceiling(g[,3]))  #similarly, ceiling calculate ceiling(a)
head(g,20) #print first 20 obs
gm<-as.matrix(g)

value<-1:20001
keys<-as.character(-10000:10000)
#install.packages("hash") <<-only for first time
library("hash")
h<-hash(keys,value)
#hmap<-matrix(c(1:20001,-10000:10000),nrow=20001) #creat bucket mapping
#bucket<-new.env()
#with(bucket,id<-key)


range<-data.frame(r=c(-10000,10000))  #column index:: 1 the given range, our target
range$n<-floor(log10(abs(range[,1])) + 1) - 1  #column index:: 2 scientific notation: a*10^n, n store 'n'
range$a<-range[,1]/(10^(range[,2]))  #column index:: 3 scientific notation a
range$floor<- as.integer(floor(range[,3]))   #column index:: 4 range[,3] is column 'a', floor calculate floor(a) 
range$ceil<-as.integer(ceiling(range[,3]))   #column index:: 5 range[,3] is column 'a', ceiling calculate ceiling(a)   


#install.packages("digest") #package offer different hash algo
#library("digest")
#a<-sapply(t,digest,algo="xxhash32")
#install.packages("formattable") <<-only for first time
library("plyr")
library("formattable")
myhash<-myfunction(X,g){
  for(i in 1:length(X)){
  x=X[i]  
  y_upper=10000 - x
  y_lower=-10000 - x
  y<-data.frame(y=c(y_upper,y_lower)) #column index:: 1 - original number 
  y$n<-floor(log10(abs(y[,1])) + 1) - 1 #column index:: 2 - n 
  y$a<-formattable(y[,1]/(10^(y[,2])),digit = 10, format = "f")#column index:: 3 - a
  y$floor<-as.integer(floor(y[,3])) #column index:: 4 - floor(a)
  y$ceil<-as.integer(ceiling(y[,3])) #column index:: 5 - ceiling(a)
  
  f<-y[1,4] #lowerbound's floor
  c<-y[2,5] #upperbound's ceiling
  dn<-y[1,2]
  a_upper<-y[1,3]  #upperbound in a
  a_lower<-y[2,3]  #lowerbound in a
  
  g_tmp<-subset(g,floor==f & ceil==c &  n==dn & a>=a_lower & a<=a_upper)
  
  if(nrow(g_tmp)>0){
    
    y=g_tmp[,1]
    k=as.character(x+y)
    id<-laply(k,function(k){h[[k]]}) #enable hash over all elements in k and return the value in one vector
    for(i in 1:length(id)){
      m<-my.list[[id[i]]]
      n<-nrow(m)
      if(n/)
      m[n+1,]<-
      my.list[[id[i]]]<<-
      rm(m,n)
    }
    
    
    
    }
  
  
  
  
  
  
  
  }
}
