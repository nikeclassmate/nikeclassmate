#1
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
