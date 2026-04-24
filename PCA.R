library(ggplot2)
library(plotly)

PCA <- prcomp(banknote[,-1], scale = TRUE)
cumsum(PCA$sdev) / sum(PCA$sdev)

# first 3 PCs explain about 70% of the variation in the data

PCs <- PCA$rotation[,1:3]
PCscores <- PCA$x[,1:3] |> cbind(Status = banknote[1]) |> as.data.frame()

# first PC corresponds most with all variables besides length
# second PC corresponds most with length of bill
# third PC corresponds most with bottom and top margins of the bill

plot_ly(
  PCscores,
  x = PCscores[,1],
  y = PCscores[,2],
  z = PCscores[,3],
  color = ~Status,
  colors = c("blue", "red"),
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 4
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

# we can see that there appears to be a "plane" separating the two groups. this suggests LDA would be good to use later on.
