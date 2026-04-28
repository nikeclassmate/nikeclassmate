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


** interpretation -This code performs Multidimensional Scaling (MDS) to convert the dissimilarity data of car brands into a 2D perceptual map.
👉 It helps identify brand positioning, where similar brands appear closer and dissimilar brands appear farther apart. **


eigenvalues <- mds_result$eig
gof <- sum(eigenvalues[1:2]) / sum(abs(eigenvalues))
cat("Goodness of Fit:", round(gof, 3), "\n")


** interpretation - This code calculates the Goodness of Fit (GOF) of the MDS solution using eigenvalues.
👉 It shows how well the 2D map represents the original dissimilarities; values closer to 1 indicate a better fit. **


# Extract coordinates
coords <- as.data.frame(mds_result$points)
colnames(coords) <- c("Dim1", "Dim2")
coords$Brand <- brands

** interpretation - This code extracts the 2D coordinates from the MDS result and organizes them into a data frame with brand names for plotting.
👉 Check in output: look at which brands are closer (more similar) and which are far apart (more different) in the perceptual map. **


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

** interpretation - 👉 Check in output: observe clusters, distances, and axis meaning—brands close together are similar, and axis positions show economy vs luxury and practical vs sporty.
👉 Interpretation: identify competitive groups and brand positioning—e.g., luxury brands clustering together indicates strong competition, while isolated brands indicate unique market positioning. **


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


** interpretation - 👉 Check in output: higher values now indicate greater similarity between brands.
👉 Interpretation: helps analyze which brands are perceived as most alike, useful for identifying close competitors and grouping. **


# Run non-metric MDS
nmds_result <- mds(diss_obj, ndim=2, type="ordinal")

** interpretation - 👉 Check in output: focus on the stress value—lower stress means a better representation.
👉 Interpretation: shows perceptual relationships more flexibly, capturing how consumers relatively rank brand similarities rather than exact distances. **


# Stress value
cat("Stress-1:", round(nmds_result$stress, 4), "\n")

** interpretation - 👉 Check in output: lower stress (generally < 0.1 good, < 0.2 acceptable) indicates a better fit.
👉 Interpretation: shows how well the NMDS map represents the data—lower stress = more reliable perceptual mapping.**


# Shepard diagram
plot(nmds_result, plot.type="Shepard",
     main="Shepard Diagram - Non-metric MDS")

👉 Check in output: points should follow a monotonic increasing pattern close to the line.
👉 Interpretation: a good fit is indicated when points closely follow the trend line, showing the NMDS model accurately preserves rank order relationships.


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

👉 Check in output: look for the “elbow point” where stress reduction slows and whether stress falls below 0.05.
👉 Interpretation: select the dimension where adding more dimensions gives minimal improvement, ensuring a good and simple model fit.


# Configuration plot
plot(nmds_result, main="Non-Metric MDS: Automobile Brands")

👉 Check in output: observe distances and clustering of brands on the map.
👉 Interpretation: brands closer together are perceived as similar, helping identify competitive groups and positioning.



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

👉 Check in output: verify how strongly each brand is associated with different attributes (higher values = stronger association).
👉 Interpretation: helps identify which attributes define each brand’s image and supports brand positioning analysis.


# Chi-square test of independence
chisq_test <- chisq.test(brand_attr)
cat("Chi-square:", round(chisq_test$statistic, 2),
    "| df:", chisq_test$parameter,
    "| p-value:", format(chisq_test$p.value, digits=3), "\n")

👉 Check in output: if p-value < 0.05, there is a significant relationship.
👉 Interpretation: confirms whether brand perceptions differ significantly across attributes, justifying further analysis like Correspondence Analysis.


# Run Correspondence Analysis
ca_result <- CA(brand_attr, graph=FALSE)

👉 Check in output: look at inertia (variance explained) and coordinates of brands and attributes.
👉 Interpretation: helps visualize relationships between brands and attributes, showing how brands are positioned based on their associations.


# Summary of results
summary(ca_result)

👉 Check in output: focus on variance explained (inertia) by dimensions and contributions of brands/attributes.
👉 Interpretation: identifies which dimensions are most important and which brands/attributes drive the perceptual positioning.



# Eigenvalues and inertia
cat("\n--- Inertia explained ---\n")
print(ca_result$eig)

👉 Check in output: look at the percentage of inertia explained by the first few dimensions.
👉 Interpretation: shows how much variation is captured—higher inertia in first dimensions means a better, simpler representation.



# Row contributions and quality
cat("\n--- Row (Brand) Coordinates ---\n")
print(ca_result$row$coord)

👉 Check in output: see the position of each brand across dimensions.
👉 Interpretation: brands with similar coordinates are perceived similarly, helping identify competitive positioning and clusters.



cat("\n--- Row Contributions (%) ---\n")
print(round(ca_result$row$contrib, 2))

👉 Check in output: identify brands with high contribution values.
👉 Interpretation: shows which brands influence the dimensions most and drive the perceptual structure.


cat("\n--- Row Quality (cos2) ---\n")
print(round(ca_result$row$cos2, 3))

👉 Check in output: values closer to 1 indicate better representation.
👉 Interpretation: shows how well each brand is accurately represented on the perceptual map.


# Symmetric biplot

fviz_ca_biplot(
  ca_result,
  repel = TRUE,
  title = "CA Biplot: Brand × Attribute Associations"
)

👉 Check in output: observe proximity between brands and attributes and overall clustering.
👉 Interpretation: brands close to certain attributes are strongly associated, helping understand brand image and positioning.


# Column (attribute) contributions to each dimension
fviz_contrib(ca_result, choice="col", axes=1,
             title="Column Contributions to Dimension 1")
fviz_contrib(ca_result, choice="col", axes=2,
             title="Column Contributions to Dimension 2")

👉 Check in output: identify attributes with highest contribution bars for each dimension.
👉 Interpretation: shows which attributes define each dimension, helping explain the meaning of perceptual axes.


#SEM
install.packages("lavaan", dependencies=TRUE)

```{r}
library(lavaan)
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")
cov(dat)
m1a <- lm(read ~ motiv, data=dat)
(fit1a <-summary(m1a))

m1b <-   '
    read ~ 1 + motiv 
    motiv ~~ motiv
'
fit1b <- sem(m1b, data=dat)
summary(fit1b)

m2 <- '
   read ~ 1 + ppsych + motiv
   ppsych ~~ motiv
'
fit2 <- sem(m2, data=dat)
summary(fit2)

m3e <- '
  # regressions
    read ~ 1 + ppsych + motiv
    arith ~ 1 + ppsych + motiv
'
fit3e <- sem(m3e, data=dat)
summary(fit3e)
  m4b <- '
  # regressions
    read ~ 1 + ppsych + motiv
    arith ~ 1 + motiv + read + ppsych
'
fit4b <- sem(m4b, data=dat)
summary(fit4b, fit.measures=TRUE)
m6a <- '
# measurement model
adjust =~ motiv + harm + stabi
risk =~ verbal + ppsych + ses
achieve =~ read + arith + spell
# regressions
achieve ~ adjust + risk
'
fit6a <- sem(m6a, data=dat)
summary(fit6a, standardized=TRUE, fit.measures=TRUE)





# Principal Component Analysis and Factor Analysis in R


mydata<- read.csv("C:/Econometrics/Data/pca_gsp.csv")
attach(mydata)

# Define variables
X <- cbind(Ag, Mining, Constr, Manuf, Manuf_nd, Transp, Comm, Energy, TradeW, TradeR, RE, Services, Govt)

# Descriptive statistics
summary(X)
cor(X)

👉 Check in output: look at mean, range (summary) and strength/direction of correlations.
👉 Interpretation: helps understand data distribution and relationships between variables, identifying possible associations or multicollinearity.


# Principal component analysis
pca1 <- princomp(X, scores=TRUE, cor=TRUE)
summary(pca1)

1. Correlation Matrix
Focus: values > 0.7
✔️ Strong → PCA valid
❌ Weak → PCA meaningless
👉 Say:
Variables are sufficiently correlated, justifying PCA.



# Loadings of principal components
loadings(pca1)
#pca1$loadings

3. Loadings
Focus: > 0.7
👉 Example:
Manufacturing, Services → high on PC1
Agriculture → high on PC2
✔️ Interpretation:
PC1 represents industrial activity, PC2 represents primary sector.



# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

4. Scree Plot
Focus: Elbow
✔️ Interpretation:
Elbow at PC2 → retain 2 components
❌ No elbow:
No clear structure



# Biplot of score variables
biplot(pca1)

👉 Check: direction and length of vectors, clustering of points.
👉 Interpretation: variables pointing in same direction are positively related, and clusters show similar observations.

# Scores of the components
pca1$scores[1:10,]

👉 Check: values across components.
👉 Interpretation: shows how each observation positions on principal components.

# Rotation
varimax(pca1$loadings[,1:3])
promax(pca1$loadings[,1:3])


2. Variance Explained
Focus: Cumulative variance
👉 Example:
PC1 = 55%, PC2 = 20% → Total = 75%
✔️ Interpretation:
First two components capture most of the information.
❌ If <50%:
PCA does not effectively reduce dimensionality.



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

Performs Factor Analysis without rotation.
👉 Check: factor loadings.
👉 Interpretation: gives initial factors but difficult to interpret due to overlapping loadings.
Applies Varimax rotation (orthogonal).
👉 Check: clearer, higher loadings.
👉 Interpretation: improves interpretability by creating distinct, uncorrelated factors.
Performs Factor Analysis with factor scores estimation (regression method).
👉 Check: factor scores.
👉 Interpretation: shows how each observation relates to underlying factors.
Displays factor scores.
👉 Check: values across factors.
👉 Interpretation: helps in grouping and profiling observations.
Applies Promax rotation (oblique).
👉 Check: loadings and correlations between factors.
👉 Interpretation: allows correlated factors, giving a more realistic structure when variables are related.



# KMO Statistics and Bartlett's Test of Sphericity
#install.packages("REdaS")
library(REdaS)
KMOS(X)
bart_spher(X)

5. KMO & Bartlett
Test	Good Result	Meaning
KMO	> 0.7	Sampling adequate
Bartlett	p < 0.05	Correlation exists





 ##Cluster Analysis
mydata <- read.csv("~/2025-26/Jan_MSBA_/utilities.csv")
attach(mydata)

# Scatter plot 
plot(Fuel_Cost~Sales, data = mydata)
with(mydata,text(Fuel_Cost ~ Sales, labels=Company,pos=4))

👉 Check in output: observe the trend (positive/negative) and spread of points.
👉 Interpretation: shows the relationship between sales and fuel cost, and helps identify patterns or outliers among companies.


# Normalization
z <- mydata[,-c(1,1)]
z
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)

👉 Check in output: values should have mean ≈ 0 and SD ≈ 1.
👉 Interpretation: ensures variables are on the same scale, making them suitable for analyses like PCA or clustering.


# Calculate Euclidean distance matrix  
distance = dist(nor)
print(distance, digits=3)

👉 Check in output: smaller values indicate more similar observations, larger values indicate dissimilarity.
👉 Interpretation: forms the basis for clustering analysis by measuring similarity between data points.



# Hierarchical agglomerative clustering using complete linkage
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)

👉 Check in output: observe how observations merge into clusters and the height (distance) of merging.
👉 Interpretation: helps identify natural groupings of companies and decide the number of clusters based on where large jumps occur.


# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclusta<-hclust(distance,method="average")
plot(mydata.hclusta,hang=-1)

👉 Check in output: observe cluster formation and merging distances.
👉 Interpretation: shows grouping based on average similarity between clusters, giving a balanced clustering structure.


# Cluster membership
member.c = cutree(mydata.hclust,3)
member.a = cutree(mydata.hclusta,3)
table(member.c, member.a)


👉 Check in output: see how observations are grouped similarly or differently across methods.
👉 Interpretation: helps evaluate consistency between clustering methods and reliability of cluster solutions.


# Characterizing clusters means
aggregate(nor,list(member.c),mean)
aggregate(mydata[,-c(1,1)],list(member.c),mean)

👉 Check in output: compare mean values across clusters.
👉 Interpretation: helps profile each cluster, showing how groups differ in characteristics.

# Silhouette Plot
library(cluster) 
plot(silhouette(cutree(mydata.hclust,3), distance)) 

5. Silhouette
Value	Meaning
~1	Strong
~0	Weak
<0	Wrong clustering


# Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

👉 Check in output: look for the “elbow point” where the decrease in WSS slows.
👉 Interpretation: helps determine the optimal number of clusters for k-means clustering.


# K-means clustering
set.seed(123)
kc<-kmeans(nor,4)
kc
plot(Sales~D.Demand,mydata,col=kc$cluster)

👉 Check in output: cluster assignments and separation of points by color.
👉 Interpretation: shows how observations are grouped into 4 distinct clusters and how they differ based on Sales and Demand.
