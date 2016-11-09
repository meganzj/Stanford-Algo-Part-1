options(expressions=500000)
#Prepare input test file  
#g=read.table("Downloads/scc.txt")
#x<<-as.matrix(g)
remove(g)
vector.is.empty<<-function(x) {return(length(x)==0)}
'%!in%' <- function(x,y)!('%in%'(x,y))
g<-c(2,1,3,1,3,4,4,2,5,4,5,6,6,7,7,8,8,9,2,3)
x<<-matrix(g,nrow=10,ncol=2, byrow=TRUE)
#g<-c(1,4,2,8,3,6,4,7,5,2,6,9,7,1,8,5,8,6,9,3,9,7,10,2)
#x<<-matrix(g,nrow=12,ncol=2, byrow=TRUE)
#g<-c(1,2,2,3,2,4,3,4,3,5,4,1,4,13,5,6,6,7,7,8,8,9,9,6,10,9,10,11,11,8,11,12,12,13,12,14,13,10,14,15)
#x<<-matrix(g,20,2,byrow=TRUE)

G_REV<<-x[,c(2,1)]
u1<-unique(x[,1])
u2<-unique(x[,2])
u<-c(u1,u2)
n<<-length(unique(u))
remove(u1,u2,u,x)

P<<-matrix(c(rep(0,2*n)),nrow=n,ncol=2) #tracking whether vertex is explored or not
colnames(P)<-c("f","parent_vertex")
explore<<-numeric()
assign("time",0,envir=globalenv())
#Algorithm -DFS
DFS_LOOP<<-function(G){
  for(i in n : 1){
    if (i %in% explore){next}
    else {
      DFS(G,i)
    }
  }
}

DFS<<-function(G,i){
  #if(time>=n){return(p)}
  #else{
    #print(c("i=",i))
    explore<<-c(explore,i)
    #print(c("explore",explore))
    
    #P[i,2] <<- 1 # gray
    v=c(G[(G[,1] == i),2])
    #print(c("v=",v))
    if (vector.is.empty(v) ==TRUE){
      len=1
      v=i
    }
    if(vector.is.empty(v)==FALSE){
      len=length(v)
    }
    
    for(j in 1: len){
      if(v[j] %!in% explore){
        DFS(G,v[j])
        P[v[j],2] <<-i
       # print(c("child",j,"parent",i))
      }
    }
    
    time<<-time + 1
    P[i,1] <<- time
    #P[i,2] <<- 2 #black
 # } <<-else
}

DFS_LOOP(G_REV)

temp0<-matrix(1:n,n,1)
temp1<-P[,1]
colnames(temp1)<-c("before","after")
temp1<-as.data.frame(temp1)
colnames(x)<-c("vertex","edge")
X<-as.data.frame(x)
X_NEW<-merge(x=X,y=temp1,by.x="vertex",by.y="before")
remove(X)
names(X_NEW)[names(X_NEW)=="after"]<-"vertex_new"
X_NEW2<-merge(x=X_NEW,y=temp1,by.x="edge",by.y="before")
remove(X_NEW,temp1)
names(X_NEW2)[names(X_NEW2)=="after"]<-"edge_new"
G2<-as.matrix(X_NEW2)
remove(X_NEW2)
G2<-G2[,c(3,4)]
u1<-unique(G2[,1])
u2<-unique(G2[,2])
u<-c(u1,u2)
n<<-length(unique(u))
remove(u1,u2,u)

P<<-matrix(c(1:n,rep(0,2*n)),nrow=n,ncol=3) #tracking whether vertex is explored or not
colnames(P)<-c("vertex","f","parent_vertex")
explore<<-numeric()
assign("time",0,envir=globalenv())

DFS_LOOP(G2)#2nd DFS

scc_temp<-P[P[,3]==0,2]
m=length(scc_temp)
scc=numeric()
for (i in 1:(m-1)){
  scc[i]=scc_temp[i]-scc_temp[i+1]
}
scc[m]<-scc_temp[m]
scc_top_5<-tail(sort(scc),5)