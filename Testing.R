library(MASS)
library(psych) # good for finding new FA scores

testClassifer <- function(S = 1, seed = 0318, trainProportion = 0.8, dimReduc = "FA") {
  set.seed(seed)
  totalSize <- nrow(banknote)
  LDA_accuracy <- numeric(S)
  for (s in 1:S) {
    trainingSize <- round(totalSize * trainProportion) # 80% training
    trainingIndices <- c(sample(100, trainingSize/2), sample(100, trainingSize/2) + 100) # 80% of genuine, 80% of counterfeit
    testingIndices <- seq(200)[-trainingIndices]
    
    if (dimReduc == "FA") { # factor analysis
      FA <- fa(banknote[trainingIndices,-1], nfactors = 3, fm = "ml", rotate = "varimax", scores = "Bartlett")
      scores_train <- FA$scores |> as.data.frame() |> cbind(Status = banknote[trainingIndices,1])
      scores_test <- predict(FA, banknote[testingIndices, -1]) |> as.data.frame()
    }
    if (dimReduc == "PCA") { # principal component anaylsis
      PCA <- prcomp(banknote[trainingIndices, -1], scale = TRUE)
      scores_train <- PCA$x[, 1:3] |> as.data.frame() |> cbind(Status = banknote[trainingIndices, 1])
      scores_test <- predict(PCA, newdata = banknote[testingIndices, -1])[, 1:3] |> as.data.frame()
    }
    
    
    # LDA with FA
    LDA <- lda(Status ~., data = scores_train)
    
    pred_test <- predict(LDA, newdata = scores_test)$class
    
    LDA_accuracy[s] <- sum(pred_test == banknote[testingIndices,1]) / length(testingIndices)
    
  }
  full_accuracy <- mean(LDA_accuracy)
  return(full_accuracy)
}

testClassifer(S=1000, trainProportion = 0.8, dimReduc = "FA") # 99.4% accuracy. nice.
testClassifer(S=1000, trainProportion = 0.8, dimReduc = "PCA") # 97.7% accuracy...

testClassifer(S=1000, trainProportion = 0.04, dimReduc = "FA") # 94.8% : EXTREMELY high accuracy for only 4% training data (4 of each)
testClassifer(S=1000, trainProportion = 0.04, dimReduc = "PCA") # 96%

# the data is clearly extremely well structured. perhaps unrealistically so?
# maybe not a /bad/ thing, but surely unrealistic.