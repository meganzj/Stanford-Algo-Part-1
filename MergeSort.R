mymerge<-function(left,right){
  #print(left)
  #print(right)
  
  n1=length(left)
  n2=length(right)
  n=n1+n2
  k=numeric(n)
  
  i=1
  j=1
  d=1
  left[n1+1]=Inf
  right[n2+1]=Inf
  
  for (d in 1:n) {
    if (left[i]<right[j]){
      k[d]=left[i]
      i=i+1
    }
    else {
      k[d]=right[j]
      j=j+1
      inversion_ct<<-inversion_ct+n1-i+1
      #print(inversion_ct)
      #print(n1)
      #print(i)
      #print(c(inversion_ct,n1-i+1))
      #return(inversion_ct)
    }
  }
  k
  #inversion_ct
}


inversion_ct<-0
ct<-function(x){
  if(length(x)<=1) x
  else{
    b=x[1:floor(length(x)/2)]
    c=x[-(1:floor(length(x)/2))]
    #c=x[(floor(length(x)/2)+1):length(x)]
    b = ct(b)
    c= ct(c)
    mymerge(b,c)}
}

