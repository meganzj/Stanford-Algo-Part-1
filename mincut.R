filePath="KargerMincut.txt"
temp_array<-readLines(filePath)
n=length(temp_array)

temp1<<-list()
for(i in 1:n){
  temp1[[i]]=as.numeric(unlist(strsplit(temp_array[i],"\t"))) 
}

mincut<-function(temp){
  len_list<-vector()
  for(q in 1:length(temp)){
    len_list[q]=temp[[q]][1]
  }
  
  row_id<-sample(len_list,1)
  for(y in 1:length(temp)){
    if(temp[[y]][1]==row_id){
      col_id<-sample(temp[[y]][2:length(temp[[y]])],1)
    }
  }
  
  va<-vector()
  for(i in 1:length(temp)){
    if(temp[[i]][1]==row_id){
      v1= temp[[i]]
    }
  }
  v1<-v1[-1]
  
  for(j in 1:length(temp)){
    if (temp[[j]][1] %in% v1){
      for (k in 2:length(temp[[j]])){
        if(temp[[j]][k]==row_id){
          temp[[j]][k]=col_id
        }
      }
    }
  }
  
  for(j in 1:length(temp)){
    if(temp[[j]][1] ==col_id){
      for(w in 1:length(temp)){
        if(temp[[w]][1]==row_id){  
          temp[[j]]=c(temp[[j]],temp[[w]][-1])
          break
        }
      }
    }
  }
  v2<-vector()
  for(h in 1:length(temp)){
    if(temp[[h]][1] == col_id){
      for(d in 1:length(temp[[h]])){
        if(temp[[h]][d]==col_id){
          v2[d]=d
        }
      }
    }
  }
  v2<-v2[!is.na(v2)]
  v2<-v2[-1]
  
  for(g in 1:length(temp)){
    if(temp[[g]][1] == col_id){
      temp[[g]]=temp[[g]][-v2]
    }
  }
  for(x in 1:length(temp)){
    if(temp[[x]][1]==row_id){
      temp[[x]]<-NULL
      break
    }
  }
  return(temp)
}

mycut<-function(x){
  if(length(x)<3){return(x)}
  else{
    x=mincut(x)
    mycut(x)   
  }
}

mult_run<-function(x){
  temp<<-x
  y<<-mycut(temp)
  min_cut<<-length(y[[1]])-1
  return(min_cut)
}

mymin<<-vector()
for(i in 1:n){
  mymin[i]=mult_run(temp1)
}

min(mymin)
