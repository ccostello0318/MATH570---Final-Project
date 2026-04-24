library(ggplot2)
library(plotly)


# factor analysis (MLE)

FA <- factanal(banknote[,-1], factors = 3, scores = "Bartlett", rotation = "varimax")

# first 3 factors explain 70% of the variance in the data

# first loading appears to mostly be the bottom margin
# second loading is mostly the top margin and diagonal length
# third loading is most the left and right margins

FAscores <- FA$scores |> cbind(Status = banknote[1]) |> as.data.frame()

plot_ly(
  FAscores,
  x = FAscores[,1],
  y = FAscores[,2],
  z = FAscores[,3],
  color = ~Status,
  colors = c("red", "blue"),
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 4
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Factor 1"),
      yaxis = list(title = "Factor 2"),
      zaxis = list(title = "Factor 3")
    )
  )

# also seems to have a plane separating most data points. LDA also would make sense here.
# seems to be an even cleaner separation than PC