#In this script, we will test performing Clustering on the 'Iris' dataset

library(dplyr)
library(ggplot2)
library(cluster)
source('functions/custom_standardise_func.R')

iris_data <- iris %>%
    as_tibble()


#Recap from 01_PCA script
#-For the iris data, we have 4 continuous, numerical variables, and 1 categorical variable 
#-We could make up to 6 different plots to understand the relationships between variables, e.g.

iris_data_real_plot <- ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("Source of truth")



#Clustering will only work on continuous/numerical data, so drop Species (we can add this label back in after)
#-Also, standardising still recommended for K-means but not absolutely essential unlike for PCA - will leave out here for now
iris_data_for_clustering <- iris_data %>%
    select(-Species)


#Let's use the kmeans function from the cluster package to assign each observation to 1 of 3 distinct Clusters 
#See my notes for what this is doing in practice. Need to set random seed for reproducibility!
iris_clusters <- kmeans(iris_data_for_clustering, centers = 3)

#Returns a vector with each Cluster an iris observation belongs to


#Add these 'predicted' Clusters to the original data and compare:
iris_data_post_clustering <- iris_data %>%
    mutate(cluster = as.character(iris_clusters$cluster))


iris_data_post_clustering %>%
    group_by(Species, cluster) %>%
    summarise(count = n())


#Going off the plots produced in the 01_PCA script, not surprising that Setosa all observations grouped into the same Cluster as they seemed quite different.
#But there is some overlap between versicolor and virginica.


#Inspect cluster centres: 
iris_clusters$centers


iris_cluster_centres_df <- iris_clusters$centers %>%
    as_tibble() %>%
    mutate(cluster = as.character(row_number()))


#And as an aside, compute the within SS by hand to check it agrees. E.g. for Cluster 1 
#Within SS (i.e. within Cluster variance) is what K-means is trying to minimise.

iris_data_post_clustering_c1 <- iris_data_post_clustering %>%
    filter(cluster == '1')

sepal_length_centre_c1 <- iris_clusters$centers[1,1]
sepal_width_centre_c1 <- iris_clusters$centers[1,2]
petal_length_centre_c1 <- iris_clusters$centers[1,3]
petal_width_centre_c1 <- iris_clusters$centers[1,4]


within_SS_sepal_length_c1 <- sum((iris_data_post_clustering_c1$Sepal.Length - sepal_length_centre_c1)^2)
within_SS_sepal_width_c1 <- sum((iris_data_post_clustering_c1$Sepal.Width - sepal_width_centre_c1)^2)
within_SS_petal_length_c1 <- sum((iris_data_post_clustering_c1$Petal.Length - petal_length_centre_c1)^2)
within_SS_petal_width_c1 <- sum((iris_data_post_clustering_c1$Petal.Width - petal_width_centre_c1)^2)

within_SS_c1_manual <- within_SS_sepal_length_c1 + within_SS_sepal_width_c1 + within_SS_petal_length_c1 + within_SS_petal_width_c1

#Compare to:
iris_clusters$withinss[1]

#SPOT ON



#Let's plot Sepal Length vs Sepal Width again, now colour-coding based on our Clusters, and plot the Cluster means too 
#-Note: Remember that the cluster centroids are determined here based on all 4 variables, not just the 2 below...
#...which might explain why are still some 'confusing' points.
iris_data_post_clustering_plot <- ggplot(iris_data_post_clustering, aes(x = Sepal.Length, y = Sepal.Width, colour = cluster)) +
    geom_point(size = 3) +
    geom_point(data = iris_cluster_centres_df, aes(x = Sepal.Length, y = Sepal.Width, colour = cluster), size = 5.0, shape = 5) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("K = 3")


ggsave("outputs/iris_clustering_sepal_length_vs_sepal_width.png", plot = iris_data_post_clustering_plot)


#So we can see how this would be quite a powerful tool if we had truly unlabelled data.



#-------------------------------

#Repeat analysis but now use 6 Clusters instead:
iris_clusters_6 <- kmeans(iris_data_for_clustering, center = 6)

#Returns a vector with each Cluster an iris observation belongs to


#Add these 'predicted' Clusters to the original data and compare:
iris_data_post_clustering_6 <- iris_data %>%
    mutate(cluster = as.character(iris_clusters_6$cluster))


iris_cluster_6_centres_df <- iris_clusters_6$centers %>%
    as_tibble() %>%
    mutate(cluster = as.character(row_number()))


#Let's plot Sepal Length vs Sepal Width again, now colour-coding based on our Clusters, and plot the Cluster means too 
#-Note: As above, cluster centroids based on all 4 variables, not just the 2 plotted here.
iris_data_post_clustering_6_plot <- ggplot(iris_data_post_clustering_6, aes(x = Sepal.Length, y = Sepal.Width, colour = cluster)) +
    geom_point(size = 3) +
    geom_point(data = iris_cluster_6_centres_df, aes(x = Sepal.Length, y = Sepal.Width, colour = cluster), size = 5.0, shape = 5) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("K = 6")


ggsave("outputs/iris_clustering_6_sepal_length_vs_sepal_width.png", plot = iris_data_post_clustering_6_plot)





#---------------------------------------------------------------------------------------------------------------
#---Comparison: Comparing application of all clustering methods (K Means, Hierarchical and GMM - see below and notes)

#Re-define iris datasets
iris_data <- iris %>%
    as_tibble()

iris_data_for_clustering <- iris_data %>%
    select(-Species)

iris_data %>%
    group_by(Species) %>%
    summarise(count = n())

#50 of each flower species


#a) 'Real' iris plot - source of truth
iris_data_real_plot <- ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("Source of truth")




#b) K-means - starts with (K = 3 here) initial cluster centroids and gradually updates until within cluster variance is minimised
set.seed(2)
iris_data_K_means_seed_2_clustering <- kmeans(iris_data_for_clustering, centers = 3)

#Compare with actual Species
iris_data_post_K_means_seed_2_clustering <- iris_data %>%
            mutate(K_cluster = as.character(iris_data_K_means_seed_2_clustering$cluster))


#Results:
iris_data_post_K_means_seed_2_clustering %>%
    group_by(Species, K_cluster) %>%
    summarise(count = n())

#Plot
iris_data_K_means_seed_2_plot <- 
    ggplot(iris_data_post_K_means_seed_2_clustering, aes(x = Sepal.Length, y = Sepal.Width, colour = K_cluster)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("K-means with K = 3")




#c) Hierarchical Clustering - starts with each point in its own cluster and gradually merges
hcluster_iris <- hclust(iris_data_dist, method = "complete")

#View as a dendrogram:
plot(hcluster_iris)

#We can then cut the above into K Clusters (or at a given height)
iris_cluster_cut <- cutree(hcluster_iris, k = 3)


#Compare with actual Species:
iris_data_post_H_clustering <- iris_data %>%
           mutate(H_cluster = as.character(iris_cluster_cut))

#Plot
iris_data_H_clustering_plot <- 
    ggplot(iris_data_post_H_clustering, aes(x = Sepal.Length, y = Sepal.Width, colour = H_cluster)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("H cut-off at K = 3")




#d) Generative Mixture Models Clustering - fit a Gaussian mixture model to the iris data with 3 'mixture' components (i.e. 3 Gaussian models, 3 clusters)
#-Point is we want to assign the probability of each data point belonging to one of these models/iris_clusters

library(mclust)

set.seed(1)
gmm_cluster_iris <- Mclust(iris_data_for_clustering, G = 3)

iris_data_post_gmm_clustering <- iris_data %>%
    mutate(gmm_cluster = as.character(gmm_cluster_iris$classification),
           p_cluster1 = gmm_cluster_iris$z[,1],
           p_cluster2 = gmm_cluster_iris$z[,2],
           p_cluster3 = gmm_cluster_iris$z[,3])


#Results:
iris_data_post_gmm_clustering %>%
    group_by(Species, gmm_cluster) %>%
    summarise(count = n())


#Plot:
iris_data_gmm_clustering_plot <- 
    ggplot(iris_data_post_gmm_clustering, aes(x = Sepal.Length, y = Sepal.Width, colour = gmm_cluster)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ggtitle("GMM with G = 3")



#Combine everything into a single plot via patchwork - 
#--REMEMBER THOUGH THAT THE CLUSTERING IS BASED ON ALL 4 NUMERIC VARIABLES, NOT JUST THE 2 SHOWN HERE!

library(patchwork)
all_iris_plots_combined <- (iris_data_real_plot | iris_data_K_means_seed_2_plot) / (iris_data_H_clustering_plot | iris_data_gmm_clustering_plot) & 
ggsave("outputs/all_iris_plots_combined.png", plot = all_iris_plots_combined)

