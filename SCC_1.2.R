options(expressions=500000)
#Prepare input test file  
g=read.table("scc.txt")
x<<-as.matrix(g)
remove(g)
vector.is.empty<<-function(x) {return(length(x)==0)}
'%!in%' <- function(x,y)!('%in%'(x,y))
#g<-c(2,1,3,1,3,4,4,2,5,4,5,6,6,7,7,8,8,9,2,3)


u1<-unique(x[,1])
u2<-unique(x[,2])
u<-c(u1,u2)
n<<-length(unique(u))
remove(u1,u2,u)

G = vector("list", length=n)
G_REV = vector("list", length=n)
P = numeric(n)
FT = numeric(n)

for (i in 1:nrow(x)) {
  a = x[i,1]
  b = x[i,2]
  #for G
  if (is.null(G[[a]])) {
    G[[a]] = c(b)
  } else {
    G[[a]] = c(G[[a]], b)
  }
  if (is.null(G[[b]])) {
    G[[b]] = numeric()
  }
  #for G_VEV
  if (is.null(G_REV[[b]])) {
    G_REV[[b]] = c(a)
  } else {
    G_REV[[b]] = c(G_REV[[b]], a)
  }
  if (is.null(G_REV[[a]])) {
    G_REV[[a]] = numeric()
  }
}

#G_REV<<-x[,c(2,1)]

P<<-matrix(c(1:n,rep(0,2*n)),nrow=n,ncol=3) #tracking whether vertex is explored or not

colnames(P)<-c("node","f","parent_vertex")
explore<<-numeric()
assign("time",0,envir=globalenv())
#Algorithm -DFS
DFS_LOOP<<-function(G){
  counter = n 
  for(i in n : 1){
   if (i < counter) {
     counter = counter - 1000;
     print(i);
   }
    if (i %in% explore){next}
    else {
      DFS(G,i)
    }
  }
}

DFS<<-function(G,i){
     explore<<-c(explore,i)
     v=G[[i]]

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
         P[v[j]] <<- i
       }
     }
    
     time<<-time + 1
     FT[i] <<- time
}
print('Starting DFS_loop on G_REV')
DFS_LOOP(G_REV)
###################################################

FT_SORTED = numeric(n)
for (i in length(FT):1) {
  finish_time = FT[i]
  FT_SORTED[finish_time] = i
}

P = numeric(n)
FT = numeric(n)

explore<<-numeric()
assign("time",0,envir=globalenv())

print('Starting DFS_loop on G')
explore<<-numeric()

for (i in length(FT_SORTED):1) { 
  k = FT_SORTED[i]
  if (k %!in% explore) {
    DFS(G, k)
  }
}

mscc_temp = which(P==0)
scc_temp=FT[mscc_temp]
scc_temp=sort(scc_temp,decreasing=TRUE)
m=length(scc_temp)
scc=numeric()
for (i in 1:(m-1)){
  scc[i]=scc_temp[i]-scc_temp[i+1]
}
scc[m]<-scc_temp[m]
scc_top_5<-tail(sort(scc),5)