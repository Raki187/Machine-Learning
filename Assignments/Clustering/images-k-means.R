library(jpeg)

args = commandArgs(trailingOnly=TRUE)
image1 <- readJPEG(args[1])
image2 <- readJPEG(args[2])
image3 <- readJPEG(args[3])

image1_dm <- dim(image1)
image2_dm <- dim(image2)
image3_dm <- dim(image3)


image1_RGB <- data.frame(
  x = rep(1:image1_dm[2], each = image1_dm[1]),
  y = rep(image1_dm[1]:1, image1_dm[2]),
  R = as.vector(image1[,,1]),
  G = as.vector(image1[,,2]),
  B = as.vector(image1[,,3])
)

image2_RGB <- data.frame(
  x = rep(1:image2_dm[2], each = image2_dm[1]),
  y = rep(image2_dm[1]:1, image2_dm[2]),
  R = as.vector(image2[,,1]),
  G = as.vector(image2[,,2]),
  B = as.vector(image2[,,3])
)

image3_RGB <- data.frame(
  x = rep(1:image3_dm[2], each = image3_dm[1]),
  y = rep(image3_dm[1]:1, image3_dm[2]),
  R = as.vector(image3[,,1]),
  G = as.vector(image3[,,2]),
  B = as.vector(image3[,,3])
)

library(ggplot2)


plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}



p1 <- ggplot(data = image1_RGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(image1_RGB[c("R", "G", "B")])) +
  labs(title = "image1:") +
  xlab("x") +
  ylab("y") +
  plotTheme()

p2 <- ggplot(data = image2_RGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(image2_RGB[c("R", "G", "B")])) +
  labs(title = "image2:") +
  xlab("x") +
  ylab("y") +
  plotTheme()

p3 <- ggplot(data = image3_RGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(image3_RGB[c("R", "G", "B")])) +
  labs(title = "image3:") +
  xlab("x") +
  ylab("y") +
  plotTheme()

kClusters1 <- 2
kMeans1 <- kmeans(image1_RGB[, c("R", "G", "B")], centers = kClusters1)
kColours1 <- rgb(kMeans1$centers[kMeans1$cluster,])

k1 <- ggplot(data = image1_RGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours1) +
  labs(title = paste("K-Means clustering of", kClusters1, "colours for image1")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()

kClusters2 <- 3
kMeans2 <- kmeans(image2_RGB[, c("R", "G", "B")], centers = kClusters2)
kColours2 <- rgb(kMeans2$centers[kMeans2$cluster,])

k2 <- ggplot(data = image2_RGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours2) +
  labs(title = paste("K-Means clustering of", kClusters2, "colours for image2")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()

kClusters3 <- 5
kMeans3 <- kmeans(image3_RGB[, c("R", "G", "B")], centers = kClusters3)
kColours3 <- rgb(kMeans3$centers[kMeans3$cluster,])

k3 <- ggplot(data = image3_RGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours3) +
  labs(title = paste("K-Means clustering of", kClusters3, "colours for image3")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()

ggsave(filename = "D:/Machine Learning/Assignment_5/Part III/clusteredImages/image_1_edited.jpg", plot = k1)
ggsave(filename = "D:/Machine Learning/Assignment_5/Part III/clusteredImages/image_2_edited.jpg", plot = k2)
ggsave(filename = "D:/Machine Learning/Assignment_5/Part III/clusteredImages/image_3_edited.jpg", plot = k3)
