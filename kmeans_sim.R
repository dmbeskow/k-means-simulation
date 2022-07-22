# this installs and loads packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidymodels, Rfast, Rcpp)
#install.packages('Rcpp')

# Create our dataset of three clusters
# set.seed(27)

centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(cols = c(x1, x2))

# Plot initial visualization
p <- ggplot(points, aes(x1, x2)) +
  geom_point(alpha = 0.3, size = 2) + theme_classic() + ggtitle("Original Data")

print(p)

# Pause for user to view
Sys.sleep(3)

# Randomly sample points to identify initial centroids
points$centroid <- 1
points$centroid[sample(1:nrow(points),3)] <-2
centroids <- points %>% filter(centroid == 2) %>% select(x1, x2)

# Plot initial centroids
p <- ggplot(points, aes(x1, x2, size = centroid, shape = factor(centroid), color = factor(centroid)) ) +
  geom_point(alpha = 0.3) + theme_classic() + ggtitle("Identify Initial Centroids") + 
  scale_size(range = c(2,4)) + theme(legend.position="none") + 
  scale_color_manual(values=c("#999999", "red"))
print(p)

# Pause for user to view
Sys.sleep(3)
  
converged = FALSE
count <- 1

# While algorithm has not converged (centroids stop changing)
while(!converged){
  
  print(paste("Iteration", count))
  
  # Compute distance matrix
  mat <- as.matrix(points[,c('x1','x2')])
  d <- dista(mat, centroids[,c('x1','x2')])
  
  # Use min distance to identify clusters
  clust <- apply(d,1, which.min)
  
  points$cluster <- clust
  
  # Visualize assignment
  p <- ggplot(points, aes(x1, x2, size = centroid, shape = factor(centroid), color = factor(cluster))) +
    geom_point(alpha = 0.3) + theme_classic() + ggtitle("Assign Points to Cluster") + 
    scale_size(range = c(2,4)) + theme(legend.position="none")
  
  print(p)
  
  # Pause for user to view
  Sys.sleep(3)
  
  # Recalculate centroids
  new_centroids <- points  %>% group_by(cluster) %>% 
    summarise(x1 = mean(x1), x2 = mean(x2)) %>% mutate(centroid = 2) 
  
  # Check for convergence
  if(identical(centroids, new_centroids)){
    break
  } else{
    centroids = new_centroids
  }
  
  # Visualize new centroids
  points <- points %>% mutate(centroid = 1) %>% bind_rows(centroids)
  
  gg <- ggplot(points, aes(x1, x2, size = centroid, shape = factor(centroid), color = factor(cluster))) +
    geom_point(alpha = 0.3) + theme_classic() + ggtitle("Adjust Centroids to Mean of Cluster") + 
    scale_size(range = c(2,4)) + theme(legend.position="none")
  print(gg)
  
  # Pause for user to view
  Sys.sleep(3)
  
  count <- count + 1
  
}
  
