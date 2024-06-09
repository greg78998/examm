# traitement des valeurs manquantes 

install.packages("VIM", type = "binary")
library(VIM)
library(dplyr)

# visualiser les données manquants 
# imputation simple 
# impoutation multiple 
# modélisation 

data <- data.frame(
  x = sample(1:100, size = 1000, replace = TRUE), 
  y = sample(c(NA,1:50),size = 1000, replace = TRUE),
  z = sample(c(NA,1:5), size = 1000, replace = TRUE)
  )

apply(data, MARGIN = 2, function(x) sum(is.na(x)))     


na.omit()

# graph pour montre la porportion de missing
res <- VIM::aggr(data)
summary(res)$combinations

# imputation simple 
install.packages("missForest", type ="binary")
library(missForest) # imputer par des forêts aléatoires

data_ <- missForest(data)

# imputation multiple  
library(mice)
imp.mice <-mice(don, m = 100, defaultMethod ="norm.boot")