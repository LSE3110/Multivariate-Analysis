setwd("C:/Users/Mira/Desktop/Project 3")
library(readxl)

Project_3_dataset <- read_excel("C:/Users/Mira/Desktop/Project 3 dataset.xlsx")


require(robustX);
x=Project_3_dataset
x=as.matrix(x)

#plot of the data
pairs(x)

#identification of outliers
output= mvBACON(x);
points(x[ ! output$subset,], pch = 4, col = 2, cex = 1.5)
y = cbind(1:14,output$dis)
colnames(y) <- c("Index","Distance"); windows();
plot(y, main = "BACON Distances: Air pollution data (n=60)")
abline(h=output$limit, col= "red", lty=2)
points(y[ ! output$subset, ], pch = 4, col = 2, cex = 1.5)


#identifying indices of outliers
T=c()
for(i in output$dis)
  if(i > output$limit)
  {
      print(match(i, output$dis)) 
  }

#creating a vector for indices of outliers
T=c()
for(i in output$dis)
  if(i > output$limit)
  {
    T <- c(T, match(i, output$dis))
  }

# PCA
pc=princomp(x,cor=T)
print(pc)
print(summary(pc,loadings=T))
pairs(pc$scores)
windows(); plot(pc) # Scree plot
#windows();
biplot(pc)
# Outputs: 
pc$scores 
pc$sd 
pc$loadings

d = as.matrix(dist(x))
y = cmdscale(d)
plot(y[,1], y[,2], pch=19)
plot(y[,1], y[,2], type="p")
text(y[,1], y[,2], rownames(60), cex=1.0)
plot(y[,1], -y[,2], type="p")
text(y[,1], -y[,2], rownames(60), cex=1.0)


#removing outliers from the dataset

x <- x[-T, ]



