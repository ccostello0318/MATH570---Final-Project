library(ggplot2)
library(plotly)

# want to use PCA and FA to classify new points.
# small open question: should i do PCA / FA with the training set separately, or with the whole set at once?
N <- nrow(PCscores)


PC_LDA <- lda(Status ~., data = PCscores)

PC_LDA_accuracy <- sum(predict(PC_LDA)$class == PCscores[,4]) / N

FA_LDA <- lda(Status ~., data = FAscores)

FA_LDA_accuracy <- sum(predict(FA_LDA)$class == PCscores[,4]) / N

PC_LDA_accuracy
FA_LDA_accuracy
# seemingly, it is more accurate to first do FA, and the preform LDA.

