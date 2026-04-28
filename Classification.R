library(ggplot2)
library(MASS)
library(plotly)


# want to use PCA and FA to classify new points.
# small open question: should i do PCA / FA with the training set separately, or with the whole set at once?
N <- nrow(PCscores)

PCscores[which(PCscores$Status == "genuine"),-4] |> cov()
PCscores[which(PCscores$Status == "counterfeit"),-4] |> cov()

PC_LDA <- lda(Status ~., data = PCscores)

PC_LDA_accuracy <- sum(predict(PC_LDA)$class == PCscores[,4]) / N

FAscores[which(FAscores$Status == "genuine"),-4] |> cov()
FAscores[which(FAscores$Status == "counterfeit"),-4] |> cov()

FA_LDA <- lda(Status ~., data = FAscores)

FA_LDA_accuracy <- sum(predict(FA_LDA)$class == PCscores[,4]) / N

PC_LDA_accuracy
FA_LDA_accuracy
# seemingly, it is more accurate to first do FA, and the preform LDA.

graphBoundary <- function(scores, model) {
  names <- colnames(scores[,1:3])
  grid <- expand.grid(
    seq(min(scores[,1]), max(scores[,1]), length.out = 150),
    seq(min(scores[,2]), max(scores[,2]), length.out = 150),
    seq(min(scores[,3]), max(scores[,3]), length.out = 150)
  )
  colnames(grid) <- names
  
  grid$score <- predict(model, newdata = grid)$x[,1]
  boundary <- grid[abs(grid$score) < 0.02, ]  # tolerance
  
  scores$Determination <- predict(model)$class
  
  plot_ly(
    scores,
    x = scores[,1],
    y = scores[,2],
    z = scores[,3],
    color = ~Status,
    colors = c("red","blue"),
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 4
    )
  ) %>%
    add_markers(
      boundary,
      x = boundary[,1],
      y = boundary[,2],
      z = boundary[,3],
      color = I("grey"),
      size = 1,
      opacity = 0.1
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = names[1]),
        yaxis = list(title = names[2]),
        zaxis = list(title = names[3])
      )
    )  
}

graphBoundary(PCscores, PC_LDA)
graphBoundary(FAscores, FA_LDA)

