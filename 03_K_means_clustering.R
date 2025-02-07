#In this script, we will test performing Clustering on the 'Iris' dataset

library(dplyr)
library(ggplot2)
library(cluster)
source('functions/custom_standardise_func.R')

iris_data <- iris %>%
    as_tibble()


#Recap:
#-For the iris data, we have 4 continuous, numerical variables, and 1 categorical variable 
#-We could make up to 6 different plots to understand the relationships between variables, e.g.

ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20))



#Clustering will only work on continuous/numerical data, so drop Species (we can add this label back in after)
iris_data_for_clustering <- iris_data %>%
    select(-Species)


#Let's use the kmeans function from the cluster package to assign each observation to 1 of 3 distinct Clusters 
#See my notes for what this is doing in practice.
iris_clusters <- kmeans(iris_data_for_clustering, center = 3)

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


#Repeat analysis but now use 6 Clusters instead

iris_clusters_6 <- kmeans(iris_data_for_clustering, center = 6)

#Returns a vector with each Cluster an iris observation belongs to


#Add these 'predicted' Clusters to the original data and compare:
iris_data_post_clustering_6 <- iris_data %>%
    mutate(cluster = as.character(iris_clusters_6$cluster))


iris_cluster_6_centres_df <- iris_clusters_6$centers %>%
    as_tibble() %>%
    mutate(cluster = as.character(row_number()))


#Let's plot Sepal Length vs Sepal Width again, now colour-coding based on our Clusters, and plot the Cluster means too 
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



