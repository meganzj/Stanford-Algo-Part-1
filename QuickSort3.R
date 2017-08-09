qs1<-read.table("QS1.txt")
qs2<-as.matrix(qs1)
qs3<-as.vector(qs2)
#test1<-c(5, 7, 10, 8, 9, 1) #test vector fir quicksort
#options(error=recover)
myswap<-function(x,i,j){
  temp1=x[i]
  temp2=x[j]
  x[i]=temp2
  x[j]=temp1
  rm(temp2)
  rm(temp1)
  return(x)
}

myIndex<-function(x,a){
  which(x==a)
}
myQS<-function(x,l,r){
  #print(c("input x =",x,l,r))
  n=r-l+1
  #print(c("length",n))
  if(n%%2==0){
    p_Index=n/2
    p2=x[l+p_Index-1]
  }
  else{
    p_Index=(n+1)/2
    p2=x[l+p_Index-1]
  }
  #print(c("p_Index=",p_Index))
  temp=c(x[l],p2,x[r])
  #print(c("pivot candidate =",temp))
  temp=sort(temp)
  temp_pivot=temp[2]
  if(temp_pivot==x[l]){pivot=x[l]}
  if(temp_pivot==p2){
    x<-myswap(x,l,(l+p_Index-1))
    pivot=x[l]
  }
  else if(temp_pivot==x[r]){
    x<-myswap(x,l,r)
    pivot=x[l]
  }
  #print(c("swap pivot to 1st element",x))
  
  #pivot=x[l]
  #print(pivot)
  
  i=l+1
  j=l+1
  #n=r-l+1
  if(j>r){break}
  for(j in (l+1):r){
    if(x[j]>pivot){
      x[j]=x[j]
      j=j+1
    }
    else if(x[j]<pivot){
      if(i>r){break}
      else{
        x=myswap(x,i,j)
        #print(x)
        j=j+1
        i=i+1
        # print(c("increment i =",i,"increment j =",j))
      }
    }
  }
  
  x<<-myswap(x,l,(i-1))
  if(n>0){
    cnt_comp<<-cnt_comp+n-1}
  bound<<-i-1
  return(bound) 
}

quicksort<-function(l,r){
  if(r-l<1){return(x)}
  else{
    bound=myQS(x,l,r)
    quicksort(l,(bound-1))
    quicksort((bound+1),r)    
  }
}

cnt_comp=0
#x=qs3
#x=test1
r=length(x)
quicksort(1,r)
