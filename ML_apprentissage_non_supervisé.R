library(dbscan)


data(iris)
iris <- as.matrix(iris[, 1:4])
don <- iris[,1:4]
#### Classification supervisée. 

# 1 - Kmeans 

don_ <- scale(don)
gp3 <- kmeans(iris[1:100],centers=3)

predict(gp3,iris[101:150,])

names(gp3)

gp3$totss
gp3$withinss
gp3$tot.withinss
gp3$betweenss
plot(iris,col=gp3$cluster)

INTER <- 1:30
INTRA <- 1:30

for(ii in 1:30){
  tmp <- kmeans(don_,centers=ii)
  INTER[ii] <- tmp$betweens
  INTRA[ii] <- tmp$tot.withinss
}

INTER <- INTER/tmp$totss*100
INTRA <- INTRA/tmp$totss*100
plot(1:30,INTER,type="h")
points(1:30,INTRA)

# 2 - Plus proche voisins
library(dbscan)

x <- iris[, -5]
nn <- kNN(x, k = 5)

i <- 10
nn$id[i,]
plot(x, col = ifelse(1:nrow(iris) %in% nn$id[i,], "red", "black"))

# visualize the 5 nearest neighbors
plot(nn, x)

# visualize a reduced 2-NN graph
plot(kNN(nn, k = 2), x)


# 3 - CAH 

donM <- dist(don)
clhs <- hclust(donM,method = "single")

plot(clhs)
plot(sort(clhs$height,dec=T)[1:20],type="h")
abline(h=0.6)
gps <- cutree(clhs,h=0.6)
plot(don,col=gps)



clhc <- hclust(donM,method = "complete")
plot(clhc)
plot(sort(clhc$height,dec=T)[1:20],type="h")
gpc <- cutree(clhc,k=4)
plot(don,col=gpc)
table(gpc)


clha <- hclust(donM,method = "average")

plot(clha)
plot(sort(clha$height,dec=T)[1:20],type="h")
gpa <- cutree(clha,k=4)
plot(don,col=gpa)
table(gpa)

clhw <- hclust(donM,method = "ward.D2")
plot(clhw)
plot(sort(clhw$height,dec=T)[1:20],type="h")
gpw <- cutree(clhw,k=4)
plot(don,col=gpw)
table(gpw)

donM <- dist(don,method="manhattan")
clhs <- hclust(donM,method = "single")
plot(sort(clhs$height,dec=T)[1:20],type="h")
abline(h=0.7)
gps <- cutree(clhs,h=0.7)
plot(don,col=gps)
table(gps)


# 4 - Dbscan 
library(dbscan)
res <- dbscan(iris, eps = .7, minPts = 5)

pairs(iris, col = res$cluster + 1L)

plot(iris, col = res$cluster)
table(res$cluster)

hullplot(iris, res)



# 5 - Mélange de modèle
library(HDclassif)
data(crabs)
don <- crabs[, 4:ncol(crabs)]
library(mclust)
res.BIC <- mclustBIC(don, verbose = FALSE)
plot(res.BIC)
mod <- Mclust(don, x = res.BIC)
summary(mod)
 
# 6 - ACP 

library(FactoMineR)
library(Factoshiny)
data("decathlon")
deca=decathlon[,1:10]
Factoshiny(deca)

resPCA <- PCA(deca)
names(resPCA)
resPCA$var$contrib
