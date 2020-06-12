# NOTE: I realized that clustering would not be useful for this particular 
# project, so the following code never was cleaned 



# load data
library(kernlab)
library(gridExtra)
library(ggdendro)
library(magrittr)
source(file = "scripts/sample.R")


# Kmeans

college_vfold <- colleges_train %>% 
  select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr) %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))


hcl_college <- college_vfold %>% 
  crossing(method = c("complete", "average", "single")) %>% 
  # mutate(kmean = map2(train, nclust, kmeans, nstart=20)) %>% 
  mutate(hcl = map2(train, method, run_hclust), # get dendos
         dendo = map(hcl, ggdendrogram))

hcl_college %>% pluck("dendo", 5)

hcl_college <- college_vfold %>% 
  # mutate(kmean = map2(train, nclust, kmeans, nstart=20)) %>% 
  mutate(hcl = map2(train, "complete", run_hclust), # get dendos
         dendo = map(hcl, ggdendrogram))


hcl_college %>% 
  crossing(ncuts = 2:8) %>% 
  mutate(
    clusters = map2(hcl, ncuts, cut_hclust), # Cut dendo
    clust_dat = map2(train, clusters, get_cluster)
  )
run_hclust <- function(x, meth){
  return(hclust(dist(x), method = meth))
}

cut_hclust <- function(hclust_obj, ncuts){
  return(cutree(hclust_obj, ncuts))
}












, # Fit K-means
         within_ss = map_dbl(kmean, get_within_ss), # Get within-cluster SS
         clusters = map2(xmat, kmean, get_cluster))









# -------------------------------------------------------------------------

get_within_ss <- function(kmean_obj){
  return(kmean_obj$tot.withinss)
}

# Get cluster labels for the data
get_cluster <- function(x, clust_obj){
  
  if(class(clust_obj) == "kmeans"){
    clust = clust_obj$cluster
  } else {
    clust = clust_obj
  }
  
  out = x %>% 
    mutate(cluster = clust)
  
  return(out)
}

# Plot data with cluster labels
plot_cluster <- function(cluster_dat, title){
  
  plt = ggplot(cluster_dat) + 
    geom_point(aes(x1, x2, color = factor(cluster)))
  
  return(plt)
}

# Add labels to plots


label_plot <- function(plot, title){
  
  return(plot + labs(title = title))
}

