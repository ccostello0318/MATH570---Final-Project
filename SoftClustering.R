# using PCA scores

# soft clustering

library(mclust)

PCfit <- Mclust(PCscores[,1:3], G=2) # implements a Gaussian mixture model
head(PCfit$z) 

# color interpolation (for a cool graph!)
p <- PCfit$z[,1]
colors <- rgb(
  red = p,
  green = 0.5,
  blue = (1 - p)
)

plot_ly(
  x = PCscores[,1],
  y = PCscores[,2],
  z = PCscores[,3],
  type = "scatter3d",
  mode = "markers",
  text = PCscores[,4],
  hoverinfo = "text",
  marker = list(
    color = colors,
    size = 4
  )
)


# using FA


FAfit <- Mclust(FAscores[,1:3], G=2) # implements a Gaussian mixture model
head(FAfit$z) 

# color interpolation (for a cool graph!)
p <- FAfit$z[,1]
colors <- rgb(
  red = p,
  green = 0.5,
  blue = (1 - p)
)

plot_ly(
  x = FAscores[,1],
  y = FAscores[,2],
  z = FAscores[,3],
  type = "scatter3d",
  mode = "markers",
  text = FAscores[,4],
  hoverinfo = "text",
  marker = list(
    color = colors,
    size = 4
  )
)
