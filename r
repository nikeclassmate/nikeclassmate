library(MASS)
library(ggplot2)
library(ggrepel)
brands <- c("Toyota","Honda","Ford","BMW","Mercedes","Maruti","Hyundai","Audi")
dissim <- matrix(c(
  0, 2, 4, 7, 8, 5, 3, 7,
  2, 0, 4, 7, 8, 5, 3, 7,
  4, 4, 0, 6, 7, 4, 4, 6,
  7, 7, 6, 0, 2, 8, 7, 2,
  8, 8, 7, 2, 0, 9, 8, 3,
  5, 5, 4, 8, 9, 0, 2, 8,
  3, 3, 4, 7, 8, 2, 0, 7,
  7, 7, 6, 2, 3, 8, 7, 0
), nrow=8, dimnames=list(brands,brands))
mds_result <- cmdscale(dissim, k=2, eig=TRUE)

eigenvalues <- mds_result$eig
gof <- sum(eigenvalues[1:2]) / sum(abs(eigenvalues))
cat("Goodness of Fit:", round(gof, 3), "\n")

# Extract coordinates
coords <- as.data.frame(mds_result$points)
colnames(coords) <- c("Dim1", "Dim2")
coords$Brand <- brands

# Plot perceptual map
ggplot(coords, aes(x=Dim1, y=Dim2, label=Brand)) +
  geom_point(size=4, color="steelblue") +
  geom_text_repel(size=4, fontface="bold") +
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
  geom_vline(xintercept=0, linetype="dashed", alpha=0.5) +
  labs(
    title="Perceptual Map of Automobile Brands",
    subtitle=paste("Metric MDS | GoF =", round(gof,3)),
    x="Dimension 1 (Economy ← → Luxury)",
    y="Dimension 2 (Practical ← → Sporty)"
  )
#2
library(smacof)
# Convert to dissimilarity object
dissim <- matrix(c(
  0, 2, 4, 7, 8, 5, 3, 7,
  2, 0, 4, 7, 8, 5, 3, 7,
  4, 4, 0, 6, 7, 4, 4, 6,
  7, 7, 6, 0, 2, 8, 7, 2,
  8, 8, 7, 2, 0, 9, 8, 3,
  5, 5, 4, 8, 9, 0, 2, 8,
  3, 3, 4, 7, 8, 2, 0, 7,
  7, 7, 6, 2, 3, 8, 7, 0
), nrow=8, dimnames=list(brands,brands))

dissim

diss_obj <- max(dissim) - dissim
diag(diss_obj) <- 0
# Run non-metric MDS
nmds_result <- mds(diss_obj, ndim=2, type="ordinal")

# Stress value
cat("Stress-1:", round(nmds_result$stress, 4), "\n")

# Shepard diagram
plot(nmds_result, plot.type="Shepard",
     main="Shepard Diagram - Non-metric MDS")

# Stress by dimensions (scree plot)
stress_vals <- numeric(5)
for(d in 1:5){
  fit <- mds(diss_obj, ndim=d, type="ordinal")
  stress_vals[d] <- fit$stress
}
plot(1:5, stress_vals, type="b", pch=19, col="tomato",
     xlab="Number of Dimensions", ylab="Stress",
     main="Scree Plot: Stress vs. Dimensions")
abline(h=0.05, lty=2, col="gray50")

# Configuration plot
plot(nmds_result, main="Non-Metric MDS: Automobile Brands")
#3
library(FactoMineR)
library(factoextra)
# Brand-Attribute contingency table
brand_attr <- matrix(c(
  95, 30, 15,  5, 10, 20,   # Brand A
  20, 80, 40, 10, 25, 30,   # Brand B
  10, 15, 90,  5, 60, 35,   # Brand C
  5, 10, 20, 95, 15, 25,   # Brand D
  15, 20, 30, 10, 80, 70    # Brand E
), nrow=5, byrow=TRUE)

rownames(brand_attr) <- c("BrandA","BrandB","BrandC","BrandD","BrandE")
colnames(brand_attr) <- c("Affordable","Reliable","Stylish","EcoFriendly","Premium","Innovative")

# Chi-square test of independence
chisq_test <- chisq.test(brand_attr)
cat("Chi-square:", round(chisq_test$statistic, 2),
    "| df:", chisq_test$parameter,
    "| p-value:", format(chisq_test$p.value, digits=3), "\n")

# Run Correspondence Analysis
ca_result <- CA(brand_attr, graph=FALSE)

# Summary of results
summary(ca_result)

# Eigenvalues and inertia
cat("\n--- Inertia explained ---\n")
print(ca_result$eig)

# Row contributions and quality
cat("\n--- Row (Brand) Coordinates ---\n")
print(ca_result$row$coord)

cat("\n--- Row Contributions (%) ---\n")
print(round(ca_result$row$contrib, 2))

cat("\n--- Row Quality (cos2) ---\n")
print(round(ca_result$row$cos2, 3))

# Symmetric biplot

fviz_ca_biplot(
  ca_result,
  repel = TRUE,
  title = "CA Biplot: Brand × Attribute Associations"
)

# Column (attribute) contributions to each dimension
fviz_contrib(ca_result, choice="col", axes=1,
             title="Column Contributions to Dimension 1")
fviz_contrib(ca_result, choice="col", axes=2,
             title="Column Contributions to Dimension 2")



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
