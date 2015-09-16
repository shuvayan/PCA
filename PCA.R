# Principal Component Analysis and Factor Analysis in R:
# Example with only two variables:
rm(list = ls())
library(calibrate)
my.classes = read.csv("http://steviep42.bitbucket.org/YOUTUBE.DIR/marks.dat")
plot(my.classes,cex=0.9,col="blue",main="Plot of Physics Scores vs. Stat Scores")
options(digits=3)
par(mfrow=c(1,1))

# Scale the data
standardize <- function(x) {(x - mean(x))}
my.scaled.classes = apply(my.classes,2,function(x) (x-mean(x)))
plot(my.scaled.classes,cex=0.9,col="blue",main="Plot of Physics Scores vs. Stat Scores",
     sub="Mean Scaled",xlim=c(-30,30))

# Find Eigen values of correlation matrix:
# Here the variables are on the same scale,so we can use covariance also.

# Manually do PCA :
my.cov = cov(my.scaled.classes)
my.eigen = eigen(my.cov)
rownames(my.eigen$vectors)=c("Physics","Stats")
colnames(my.eigen$vectors)=c("PC1","PC2")

# Note that the sum of the eigen values equals the total variance of the data

sum(my.eigen$values)
var(my.scaled.classes[,1]) + var(my.scaled.classes[,2])

# The Eigen vectors are the principal components. We see to what extent each variable contributes

loadings = my.eigen$vectors

# Let's plot them:
pc1.slope = my.eigen$vectors[1,1]/my.eigen$vectors[2,1]
pc2.slope = my.eigen$vectors[1,2]/my.eigen$vectors[2,2]

abline(0,pc1.slope,col="red")
abline(0,pc2.slope,col="green")

textxy(12,10,"(-0.710,-0.695)",cx=0.9,dcol="red")
textxy(-12,10,"(0.695,-0.719)",cx=0.9,dcol="green")

# See how much variation each eigenvector accounts for

pc1.var = 100*round(my.eigen$values[1]/sum(my.eigen$values),digits=2)
pc2.var = 100*round(my.eigen$values[2]/sum(my.eigen$values),digits=2)
xlab=paste("PC1 - ",pc1.var," % of variation",sep="")
ylab=paste("PC2 - ",pc2.var," % of variation",sep="")

# Multiply the scaled data by the eigen vectors (principal components)

scores = my.scaled.classes %*% loadings
sd = sqrt(my.eigen$values)
rownames(loadings) = colnames(my.classes)

plot(scores,ylim=c(-10,10),main="Data in terms of EigenVectors / PCs",xlab=xlab,ylab=ylab)
abline(0,0,col="red")
abline(0,90,col="green")

# Correlation BiPlot

scores.min = min(scores[,1:2])
scores.max = max(scores[,1:2])

plot(scores[,1]/sd[1],scores[,2]/sd[2],main="My First BiPlot",xlab=xlab,ylab=ylab,type="n")
rownames(scores)=seq(1:nrow(scores))
abline(0,0,col="red")
abline(0,90,col="green")

# This is to make the size of the lines more apparent
factor = 5

# First plot the variables as vectors
arrows(0,0,loadings[,1]*sd[1]/factor,loadings[,2]*sd[2]/factor,length=0.1, lwd=2,angle=20, col="red")
text(loadings[,1]*sd[1]/factor*1.2,loadings[,2]*sd[2]/factor*1.2,rownames(loadings), col="red", cex=1.2)

# Second plot the scores as points
text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores),col="blue", cex=0.7)

somelabs = paste(round(my.classes[,1],digits=1),round(my.classes[,2],digits=1),sep=" , ")
#identify(scores[,1]/sd[1],scores[,2]/sd[2],labels=somelabs,cex=0.8)


# PCA - 2:
# Using prcomp and varimax for PCA in R www.youtube.com/watch?v=PSuvMBtvJcA 
library(lattice)
my.wines <- read.csv("http://steviep42.bitbucket.org/YOUTUBE.DIR/wines.csv", header=TRUE)

# Look at the correlations

library(gclus)
my.abs     <- abs(cor(my.wines[,-1]))
my.colors  <- dmat.color(my.abs)
my.ordered <- order.single(cor(my.wines[,-1]))
cpairs(my.wines, my.ordered, panel.colors=my.colors, gap=0.5)

# Do the PCA 

my.prc <- prcomp(my.wines[,-1], center=TRUE, scale=TRUE)
screeplot(my.prc, main="Scree Plot", xlab="Components")
screeplot(my.prc, main="Scree Plot", type="line" )

# DotPlot PC1

load    <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# DotPlot PC2

sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2"
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# Now draw the BiPlot
biplot(my.prc, cex=c(1, 0.7))

# Apply the Varimax Rotation
my.var <- varimax(my.prc$rotation)


# PCA -3:
pca.data <- read.csv("/home/shuvayan/Downloads/pca_gsp.csv")
attach(pca.data)

# Define variables
X <- cbind(Ag, Mining, Constr, Manuf, Manuf_nd, Transp, Comm,
           Energy, TradeW, TradeR, RE, Services, Govt)

# Descriptive statistics
summary(X)

# Principal component analysis should not be applied if the data is not highly correlated.
pca1 <- princomp(X, scores=TRUE, cor=T)
pca1_rot <- prcomp(X,center = T,scale. = T)
summary(pca1)
(1.7987525^2+ 1.4954801^2+ 1.3999420^2 +1.1663403^2+ 1.07583525^2+ 0.93184458^2+ 0.85116719^2
+0.78471605^2+ 0.5641253^2+ 0.4851322^2+ 0.38943836^2+ 0.36945813^2+ 8.279806e-03^2)

mean(1.7987525^2,1.4954801^2, 1.3999420^2 ,1.1663403^2, 1.07583525^2, 0.93184458^2,0.85116719^2
,0.78471605^2, 0.5641253^2, 0.4851322^2,0.38943836^2, 0.36945813^2,8.279806e-03^2)

# Loadings of principal components
loadings(pca1)
#pca1$loadings:
pca1$loadings[2,1]^2+pca1$loadings[2,2]^2+pca1$loadings[2,3]^2+pca1$loadings[2,4]^2+pca1$loadings[2,5]^2+
  pca1$loadings[2,6]^2+pca1$loadings[2,7]^2+pca1$loadings[2,8]^2+pca1$loadings[2,9]^2+pca1$loadings[2,10]^2+
  pca1$loadings[2,11]^2+pca1$loadings[2,12]^2+pca1$loadings[2,13]^2
pca1$loadings[1,1]^2+pca1$loadings[1,2]^2+pca1$loadings[1,3]^2+pca1$loadings[1,4]^2+pca1$loadings[1,5]^2+pca1$loadings[1,6]^2+pca1$loadings[1,7]^2+pca1$loadings[1,8]^2+pca1$loadings[1,9]^2+pca1$loadings[1,10]^2+pca1$loadings[1,11]^2+pca1$loadings[1,12]^2+pca1$loadings[1,13]^2

# Sum of the squared components of each PCA is equal to 1:Trying to capture the maximum variance
sum(pca1$loadings[,1]^2)
# Scree plot of eigenvalues:
plot(pca1,type = "lines")
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables:
biplot(pca1)

# Scores of the components
pca1$scores[1:10,]

# Rotation
pca1.rotate <- varimax(pca1$loadings)
#promax(pca1$rotation)



