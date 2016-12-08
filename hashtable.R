#Very Useful package, if you want to find what package does a function belongs, use finFn() in 'sos'
#install.packages("sos")
#library("sos")
#findFn("laply")   
'%!in%' <- function(x, y) {
  ! ('%in%'(x, y))
}
####################################################################################################
g = read.table("hash2sum.txt")
g=data.frame(sort(g[,1]))
g.size<-nrow(g)
#x<-as.vector(g[,1]) #convert dataframe to vector
colnames(g)[1]="x"
g$n<-floor(log10(abs(g[,1])) + 1) - 1 #scientific notation: a*10^n, n store 'n'
g$a<-g[,1]/(10^(g[,2])) #Scientific notation: a * 10^n, 'a' stores 'a'
g$floor<-as.integer(floor(g[,3]))   #g[,3] is column 'a', floor calculate floor(a) 
g$ceil<-as.integer(ceiling(g[,3]))  #similarly, ceiling calculate ceiling(a)
#head(g,20) #print first 20 obs
#gm<-as.matrix(g)

range<-data.frame(r=c(-10000,10000))  #column index:: 1 the given range, our target
range$n<-floor(log10(abs(range[,1])) + 1) - 1  #column index:: 2 scientific notation: a*10^n, n store 'n'
range$a<-range[,1]/(10^(range[,2]))  #column index:: 3 scientific notation a
range$floor<- as.integer(floor(range[,3]))   #column index:: 4 range[,3] is column 'a', floor calculate floor(a) 
range$ceil<-as.integer(ceiling(range[,3]))   #column index:: 5 range[,3] is column 'a', ceiling calculate ceiling(a)   

#install.packages("hash") <<-only for first time
bucket.size<-range[2,1]-range[1,1] + 1
value<-1:bucket.size
keys<-as.character(range[2,1]:range[1,1])

library("hash")
h<-hash(keys,value)

default.size<- ceiling(g.size/bucket.size) * 2  #assign defult size of within each bucket
                                                #in this case, 1M obs/200001 buckets  = 5
                                                #expand 4 times in case some bucket have more match pairs
my.matrix<-matrix(0,default.size,2)             #defult data structure within each bucket   
my.list<<- rep(list(my.matrix),bucket.size)     #container hold 

#install.packages("digest") #package offer different hash algo
#library("digest")
#a<-sapply(t,digest,algo="xxhash32")
#install.packages("formattable") <<-only for first time
library("plyr")
library("formattable")

myhash<-function(g){
    for(i in 1:length(g[,1])){
    x=g[i,1] 
    print(c("x=",x))
    y_upper=10000 - x
    y_lower=-10000 - x
    y<-data.frame(y=c(y_upper,y_lower)) #column index:: 1 - original number 
    y$n<-floor(log10(abs(y[,1])) + 1) - 1 #column index:: 2 - n 
    y$a<-formattable(y[,1]/(10^(y[,2])),digit = 10, format = "f")#column index:: 3 - a
    y$floor<-as.integer(floor(y[,3])) #column index:: 4 - floor(a)
    y$ceil<-as.integer(ceiling(y[,3])) #column index:: 5 - ceiling(a)
    print(c("y=",y))
    f<-y[1,4] #lowerbound's floor
    c<-y[2,5] #upperbound's ceiling
    dn<-y[,2]
    a_upper<-y[1,3]  #upperbound in a
    a_lower<-y[2,3]  #lowerbound in a
  
    #g_tmp<-which(subset(g,floor==f & ceil==c &  n %in% dn & a>=a_lower & a<=a_upper)[1] > x)
    y<-unlist(subset(g,floor==f & ceil==c &  n %in% dn & a>=a_lower & a<=a_upper)[1])
    #print(y)
    if(length(y)>0){
      y<-y[y>x]
    }
    #print(c("y=",y))
    print(length(y)==0)
    #print(c("g_tmp",y))
    #print(length(y))
    #if(nrow(g_tmp)==0){print("ZERO MATH PAIR FOR X = ")}
    if(length(y)==0){print("ZERO MATH PAIR FOR X = ")}
    #if(nrow(g_tmp)==0){next}
    if(length(y)==0){next}
    else{
        #y=g_tmp[which(g_tmp[,1]>=x),1]
        k=as.character(x+y)
        id<-laply(k,function(k){h[[k]]}) #enable hash over all elements in k and return the value in one vector
        for(i in 1:length(id)){
            m<-my.list[[id[i]]]
            n<-nrow(m)
                if(m[n,1]>0 | m[n,2]>0){
                    m<-rbind(m,my.matrix)
                    n<-nrow(m)
                }
            if (any(duplicated(x,MARGIN=1))==TRUE){next} #check duplicate
            else{
                q<-which(m[,1]==0 & m[,2]==0)[1] #find the next empty row to store new x,y pair
                m[q,]<- c(x,y[i]) #assign new pair into matrix m
                my.list[[id[i]]]<<- m  #assign the updated matrix m back to its corresponding list location
                rm(m,n)
            }
       }
    }
  }
}
myhash(g)
