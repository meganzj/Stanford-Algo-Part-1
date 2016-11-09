filePath="dijkstraData.txt"
#filePath="testData.txt"
temp_array<-readLines(filePath)
n=length(temp_array)

########step 0:little function help for the algo
sameAs<-function(x,y){
  a<-unique(x)
  b<-unique(y)
  return(setequal(a,b))
  remove(a,b)
} #check if two vector's element are the same, ignore frequency. In this case,
#help to check if algo go through all vertices

'%!in%' <- function(x, y) {
  ! ('%in%'(x, y))
}
########step 1:Prepare input data for step 2
temp1<<-list()
for(i in 1:n){
  temp1[[i]]=unlist((strsplit(temp_array[i],"\t")))
}

########step 2: convert input data into list; each list element is a matrix have two column
########1st column indicates edges, 2nd column indicate weight
temp2<-as.numeric()
temp3<-list()
for(i in 1:n){
  temp2<-as.numeric(unlist(strsplit(as.character(temp1[[i]]),",")))
  temp2<-temp2[-1]
  len=length(temp2)/2
  temp3[[i]]= matrix(temp2,len,2,byrow=TRUE)
}

#######step 3:prepare those data structues for dijkstra algo to use
v<<-1:n # all vertices in graph G; use as benchmark in the following dijkstra algo
x<<-as.numeric() #contain vertices processed SO FAR
temp_x<-as.numeric() #contain vertices being processed RIGHT NOW
A<<-vector(mode="numeric",length=n) #contain shortest path distances
B<<-vector(mode="numeric",length=n)
x[1]=1 #set up source vertex
B[1]=1 #set up initial value

#######step 4:build dijkstra algo
dijkstra<-function(G){
  while(sameAs(x,v) == FALSE){
    
    temp_x<-x
    currentlen<-length(temp_x)
    
    k<-as.numeric()
    l<-as.numeric()
    r<-as.numeric()
    
    for (i in 1:currentlen){
      u1<-G[[temp_x[i]]] #load all edges and distance for vertices in current run
      ####should add checking process for u1 to only keep cross edges
      u<-u1[u1[,1] %!in% x,]
      ####
      if(length(u)==0){
        l[i]<-Inf
        r[i]<-NA
        k[i]<-Inf
      }
      else{
        print("u=")
        print(u)
       
        if(is.vector(u)==TRUE){temp_g<-u}
          else{temp_g<-u[which(u[,2]==min(u[,2])),]}
        
          if(is.matrix(temp_g==TRUE)){
            temp_g<-temp_g[1,]
          }
          else {temp_g<-temp_g}
        
        l[i]<-temp_g[2]
        r[i]<-temp_g[1]
      }
    }
    A_temp<-as.numeric()
    for (i in 1:currentlen){
      A_temp[i]<-A[temp_x[i]]
    }
    k=A_temp + l
    #find edge has min distance for vertex i(i are qualified vertices stored in u)
    #calculate distance for edge candidates;each vertex would only have on edge candidates
    Ind1<-which(k==min(k)) #choose the vertex have smallest edge
    Ind1<-Ind1[1]
    print(c("global shortest",Ind1))
    addon<-r[Ind1]#read value of the edge
    A[addon]<<-min(k) #write distance to vector A at position temp_x[Ind], this mean we pick the vertex have value temp_x[Ind]
    B[addon]<<-addon
    x<<-c(x,addon)
  }
}

#######step 5:run dijkstra algo on given graph  
dijkstra(temp3)