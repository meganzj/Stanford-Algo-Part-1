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
gm<-as.vector(g[,1]) #convert dataframe to vector
# colnames(g)[1]="x"
# g$n<-floor(log10(abs(g[,1])) + 1) - 1 #scientific notation: a*10^n, n store 'n'
# g$a<-g[,1]/(10^(g[,2])) #Scientific notation: a * 10^n, 'a' stores 'a'
# g$floor<-as.integer(floor(g[,3]))   #g[,3] is column 'a', floor calculate floor(a) 
# g$ceil<-as.integer(ceiling(g[,3]))  #similarly, ceiling calculate ceiling(a)
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
#######test hash#######
value<-1:41
keys<-as.character(-20:20)

library("hash")
h<-hash(keys,value)

#default.size<- ceiling(g.size/bucket.size) * 2  #assign defult size of within each bucket
#in this case, 1M obs/200001 buckets  = 5
#expand 4 times in case some bucket have more match pairs
# my.matrix<-matrix(0,default.size,2)             #defult data structure within each bucket   
# my.list<<- rep(list(my.matrix),bucket.size)     #container hold 
my.check<<-numeric(bucket.size)
#my.check<<-numeric(201)
#install.packages("digest") #package offer different hash algo
#library("digest")
#a<-sapply(t,digest,algo="xxhash32")
#install.packages("formattable") <<-only for first time
library("plyr")

myhash<-function(gm,l,u){
  for(i in 1:length(gm)){
    x=gm[i] 
    #print(c("x=",x))
    y_upper=u - x
    y_lower=l - x
    t<-as.character(y_lower,y_upper)
    temp<-as.vector(laply(k,function(k){h[[k]]}))
    y<-gm[gm>=max(y_lower,x) & gm <= y_upper ]
    if(length(y)==0){next}
    else{
      #print("FIND A MATCH ")
      print(x)
      #y=g_tmp[which(g_tmp[,1]>=x),1]
      k=as.character((x+y))
      id<-as.vector(laply(k,function(k){h[[k]]})) #enable hash over all elements in k and return the value in one vector
      #print(c("id=",id))
      id<-id[which(id %!in% my.check)]
      #print(c("id=",id))
      if(length(id) == 0) {next}
      else{
        q<-which(my.check ==0)[1]
        for (i in 1:length(id)){
          my.check[q+i-1] <<- id[i] 
        }
     }
  }
}
}
myhash(gm,-10000,10000)
