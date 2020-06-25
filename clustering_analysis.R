setwd("C:/Users/Mira/Desktop/project 4")
Project_4 <- read_excel("C:/Users/Mira/Desktop/Project 4.xlsx")
x=Project_4
x=as.matrix(x)

pairs(x)

#Scaling the data

x=scale(x)

#Measuring distances between observations

#euclidean
D1=(dist(x))
D2=(dist(x, method = "manhattan"))
#canberra
D3=(dist(x, method = "canberra"))
#maximum
D4=(dist(x, method = "maximum"))

#clustering using Hierarchical clustering

hcs1 = hclust(D1,method ="single")
hcs2 = hclust(D2,method ="single")
hcs3 = hclust(D3,method ="single")
hcs4 = hclust(D4,method ="single")

plot(hcs1)
plot(hcs2)
plot(hcs3)
plot(hcs4)

hcw1 = hclust(D1,method ="ward.D2")
hcw2 = hclust(D2,method ="ward.D2")
hcw3 = hclust(D3,method ="ward.D2")
hcw4 = hclust(D4,method ="ward.D2")

plot(hcw1)
plot(hcw2)
plot(hcw3)
plot(hcw4)

hcc1 = hclust(D1,method ="complete")
hcc2 = hclust(D2,method ="complete")
hcc3 = hclust(D3,method ="complete")
hcc4 = hclust(D4,method ="complete")

plot(hcc1)
plot(hcc2)
plot(hcc3)
plot(hcc4)

hca1 = hclust(D1,method ="average")
hca2 = hclust(D2,method ="average")
hca3 = hclust(D3,method ="average")
hca4 = hclust(D4,method ="average")

plot(hca1)
plot(hca2)
plot(hca3)
plot(hca4)

clusters=cutree(hcw2, k=3) # cut tree into k clusters
rect.hclust(hcw2, k=2, border="red")

centers=aggregate(x, list(clusters), mean)

list(clusters)
clusters; table(clusters)
centers


# Determining the number of clousters
x=scale(x,center=T,scale=T); # scale x first
wss = (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:10) {
  wss[i] = sum(kmeans(x,centers=i)$withinss)}
plot(1:10, wss, type="b", xlab="k", ylab="WSS",
     main="The L-Curve")

#
k=2; kmc = kmeans(x, k);
clusters=kmc$cluster
plot(x, col = kmc$cluster)
points(kmc$centers, col = 1:k, pch = 8, cex=2)
points(kmc$centers, col = 1:k, pch = 19, cex=1)
clusters; table(clusters)
kmc$centers


R2=function(x,clusters,k){
  n=nrow(x); tss=var(x); tss=(n-1)*sum(diag(tss));
  wss=0
  for(j in 1:k){
    cj=x[clusters==j,]; nj=nrow(cj);
    vj=var(cj); wssj=0
    if(is.matrix(cj)) wssj=(nj-1)*sum(diag(vj));
    wss=wss+wssj
  }
  r2=1-wss/tss; cat("R2 = ",r2,"\n")
  return(r2)
}
r2=R2(x,clusters,3)





