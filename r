# Principal Component Analysis and Factor Analysis in R


mydata<- read.csv("C:/Econometrics/Data/pca_gsp.csv")
attach(mydata)

# Define variables
X <- cbind(Ag, Mining, Constr, Manuf, Manuf_nd, Transp, Comm, Energy, TradeW, TradeR, RE, Services, Govt)

# Descriptive statistics
summary(X)
cor(X)

# Principal component analysis
pca1 <- princomp(X, scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

# Scores of the components
pca1$scores[1:10,]

# Rotation
varimax(pca1$loadings[,1:3])
promax(pca1$loadings[,1:3])


# Factor analysis 
fa1 <- factanal(X, factor=3, rotation="none")
fa1

fa2 <- factanal(X, factor=3, rotation="varimax")
fa2

fa3 <- factanal(X, factors=3, rotation="varimax", scores="regression")
fa3
fa3$scores

fa4 <- factanal(X, factor=3, rotation="promax")
fa4

# KMO Statistics and Bartlett's Test of Sphericity
#install.packages("REdaS")
library(REdaS)
KMOS(X)
bart_spher(X)
 ##Cluster Analysis
mydata <- read.csv("~/2025-26/Jan_MSBA_/utilities.csv")
attach(mydata)

# Scatter plot 
plot(Fuel_Cost~Sales, data = mydata)
with(mydata,text(Fuel_Cost ~ Sales, labels=Company,pos=4))

# Normalization
z <- mydata[,-c(1,1)]
z
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)

# Calculate Euclidean distance matrix  
distance = dist(nor)
print(distance, digits=3)

# Hierarchical agglomerative clustering using complete linkage
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclusta<-hclust(distance,method="average")
plot(mydata.hclusta,hang=-1)

# Cluster membership
member.c = cutree(mydata.hclust,3)
member.a = cutree(mydata.hclusta,3)
table(member.c, member.a)

# Characterizing clusters means
aggregate(nor,list(member.c),mean)
aggregate(mydata[,-c(1,1)],list(member.c),mean)

# Silhouette Plot
library(cluster) 
plot(silhouette(cutree(mydata.hclust,3), distance)) 

# Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
set.seed(123)
kc<-kmeans(nor,4)
kc
plot(Sales~D.Demand,mydata,col=kc$cluster)
