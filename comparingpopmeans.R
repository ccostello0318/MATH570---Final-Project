# 2 population comparison

genuineIndex <- which(banknote == "genuine")
counterfeitIndex <- which(banknote == "counterfeit")
varnames <- colnames(banknote[,-1])

Sigma1 <- banknote[genuineIndex,-1] |> cov()
Sigma2 <- banknote[counterfeitIndex,-1] |> cov()

# we can use Box's M Test 
library(biotools)
boxM(banknote[,-1], banknote[,1])

# strong evidence they differ.

# size is small.

# is the data at least normal?

for (i in 2:7) {
  hist(banknote[genuineIndex,i],
       main = paste("Distribution of ",varnames[i-1], " of Genuine"),
       xlab = varnames[i-1]
  )
  hist(banknote[counterfeitIndex, i],
       main = paste("Distribution of ",varnames[i-1], " of Counterfeit"),
       xlab = varnames[i-1]
       )
}

  # for the most part, the data is normal. we will assume normality.

trace <- function(M) sum(diag(M))

unequalCovPopulationCompare <- function(data1 = banknote[genuineIndex,-1], data2 = banknote[counterfeitIndex,-1], vars = varnames, delta = rep(0, length(varnames))) {
  if (length(vars) != length(delta)) { return("error1!") }
  if (any(!(vars %in% varnames))) {return("error2!") }
  
  data1 <- data1[,vars]
  data2 <- data2[,vars]
  
  barX1 <- colMeans(data1)
  barX2 <- colMeans(data2)
  meanDifference <- barX1 - barX2 - delta
  
  S1 <- cov(data1)
  S2 <- cov(data2)
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  
  Sp_inv <- solve(S1 / n1 + S2 / n2)
  
  TestStatistic <- t(meanDifference) %*% Sp_inv %*% meanDifference
  
  # from JW textbook (6-29) pg. 294
  p <- length(vars)
  
  A1 <- 1 / n1 * S1 %*% solve( S1 / n1 + S2 / n2 )
  A2 <- 1 / n2 * S2 %*% solve( S1 / n1 + S2 / n2 )
  
  nu <- ( p + p^2 ) / ( 1 / n1 * ( trace(A1 %*% A1) + trace(A1)^2 ) +
                        1 / n2 * ( trace(A1 %*% A1) + trace(A2)^2 ) )
  
  HotellingT2Coefficent <- nu * p / (nu - p + 1)
  HotellingT2 <- HotellingT2Coefficent * qf(0.95, p, nu - p + 1)
  
  return(list(
      TestStatistic = TestStatistic,
      nu = nu,
      HotellingT2 = HotellingT2
      )
    )
}

