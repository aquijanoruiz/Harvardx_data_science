#---------------Contents---------------
#1. Hierarchical clustering
#2. k means
#3. heatmaps
#3.1 Filtering the information of the heatmap

#-------------------Preparing the data--------------------

#A first step in any clustering algorithm is defining a distance between observations or groups of 
#observations. Then we need to decide how to join observations into clusters. There are many 
#algorithms for doing this. Here we introduce two as examples: hierarchical and k-means.

library(dslabs)
data("movielens")

top <- movielens %>% #we choose the 50 movies witht he most reviews
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)

x <- movielens %>% #we choose the users that have wrten more than 25 reviews
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>%
  select(title, userId, rating) %>%
  spread(userId, rating) #we create a matrix: movie names as rows, users as columns

view(x)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix() #we eliminate the first column (user 8)

x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

#To know what the str_trunc() function does:
string <- "This string is moderately long"
rbind(
  str_trunc(string, 20, "right"),
  str_trunc(string, 20, "left"),
  str_trunc(string, 20, "center")
)


#----------------------1. Hierarchical clustering-------------------
###The first step is to find the distance between each pair of movies using the dist function.
d <- dist(x)

###Hierarchical clustering starts by defining each observation as a separate group, then the two closest groups
###are joined into a group iteratively until there is just one group including all the observations.

h <- hclust(d)

plot(h, cex = 0.65) #We can see the resulting groups using a dendrogram.

d <- dist(x)

###To generate actual groups we can do one of two things: 
#1) decide on a minimum distance needed for observations to be in the same group or 
#2) decide on the number of groups you want and then find the minimum distance that achieves this.

groups <- cutree(h, k = 10) #cutree can be applied to the output of hclust to perform either of these two operations and generate groups
split(names(groups), groups)

#We can also explore the data to see if there are clusters of movie raters. (so we transpose the matrix)
h_2 <- dist(t(x)) %>% hclust()
plot(h_2, cex = 0.35)

#----------------------2. k means-------------------
#To use the k-means clustering algorithm we have to pre-define k, the number of clusters we want to define.

#The kmeans function included in R-base does not handle NAs. For illustrative purposes we will fill out the
#NAs with 0s. In general, the choice of how to fill in missing data, or if one should do it at all, should be
#made with care.

x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 10)

groups <- k$cluster
split(names(groups), groups)

#Note that because the first center is chosen at random, the final clusters are random. We impose some
#stability by repeating the entire several times and averaging the results.

k <- kmeans(x_0, centers = 10, nstart = 25)
groups <- k$cluster
split(names(groups), groups)

#----------------------3. heatmaps-------------------
#A powerful visualization tool for discovering clusters or patterns in your data is the heatmap.
#It plots an image of your data matrix with colors used as the visual cue and both the columns
#and rows ordered according to the results of clustering algorithm. 

#We will demonstrate this with the tissue_gene_expression dataset.

##First step
data("tissue_gene_expression")
x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x))
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))

##Second step
image(x[h_1$order, h_2$order],
      col = RColorBrewer::brewer.pal(11, "Spectral"))

#----------------------3.1 Filtering the information of the heatmap-------------------
#There is also heatmap function that you can use on the original matrix:
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral")) 

#We do not show the results of the heatmap function because there are too many features for the plot to be
#useful. We will therefore filter some columns and remake the plos.

library(matrixStats)
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:25]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))
