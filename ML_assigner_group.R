###affectation

data("ethanol")
donnees <- ethanol

don <- donnees 

perc_apprentissage <- 0.8 

test_sample <- sample(1:10, size = nrow(don), replace = TRUE)
test_sample <- test_sample>= perc_apprentissage*10

train_set <- don[test_sample==FALSE,]
test_set <- don[test_sample==TRUE,]

INTER <- 1:30
INTRA <- 1:30

for(ii in 1:30){
  tmp <- kmeans(don,centers=ii)
  INTER[ii] <- tmp$betweens
  INTRA[ii] <- tmp$tot.withinss
}

INTER <- INTER/tmp$totss*100
INTRA <- INTRA/tmp$totss*100
plot(1:30,INTER,type="h")
points(1:30,INTRA)

# Je choisis 6 groupes

nb_cluster <- 3
tmp <- kmeans(train_set,centers=nb_cluster)

for (ii in 1:nb_cluster){
  donG <- train_set[tmp$cluster == ii,]
  centre <- colMeans(donG)
  if (ii==1){
    concat_centre <- centre
  } else {
    concat_centre <- concat_centre %>% 
      bind_rows(centre)
  }
}

concat_centre

para1 <- c(2,4) 
para3 <- c(5,8)

distance(para1,para3)

distance <- function(X,centre){
  distance_calculus <- mean((X-centre)^2)
  return(distance_calculus)
}

MAT <- matrix(NA,nrow=nrow(test_set),ncol=nb_cluster)

for(ii in 1:nrow(test_set)){
  
  for(jj in 1:nb_cluster){
    
    MAT[ii,jj] <- distance(as.numeric(test_set[ii,]),as.numeric(concat_centre[jj,]))
    
  }
}

gpT <- apply(MAT,1,which.min)
saveRDS(gpT,"gptest.RDS")


test_set$cluster <- gp
