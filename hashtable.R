g = read.table("hash2sum.txt")
x<-as.vector(g[,1]) #convert dataframe to vector
g$digit<-floor(log10(abs(g[,1])) + 1) - 1
g$temp<-g[,1]/(10^(g[,2]))
head(g,5)
range<-data.frame(r=c(-10000,10000))
range$digit<-floor(log10(abs(range[,1])) + 1) - 1
range$temp<-range[,1]/(10^(range[,2]))


a<-sapply(t,digest,algo="xxhash32")

install.packages("digest")
library("digest")