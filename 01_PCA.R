#In this script, we will test performing PCA on the 'Iris' dataset

library(dplyr)
library(ggplot2)
library(cluster)
source('functions/custom_standardise_func.R')

iris_data <- iris %>%
    as_tibble()



#-For the iris data, we have 4 continuous, numerical variables, and 1 categorical variable 
#-We could make up to 6 different plots to understand the relationships between variables, e.g.

#) Plot 1 - Sepal.Length vs Sepal.Width

iris_plot_1 <- ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Sepal Width (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20))

ggsave("outputs/iris_plot_sepal_length_vs_sepal_width.png", plot = iris_plot_1)


#Visually, relationship fairly linear, but and the 'setosa' group clearly well separated
#...but difficult to determine any discernable difference between versicolor and virginica
#...Setosa tends to be shorter and wider, with versicolor/virginica longer and narrower.

#) Plot 2 - Sepal.Length vs Petal.Length

iris_plot_2 <- ggplot(iris_data, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
    geom_point() +
    theme_bw() +
    xlab("Sepal Length (cm)") +
    ylab("Petal Length (cm)") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20))

ggsave("outputs/iris_plot_sepal_length_vs_petal_length.png", plot = iris_plot_2)

#So here, for setosa, seems to be no clear linear relationship
#For the others, clearly linear, and somewhat distinguished


#So that we don't need to try and look at all (four) numerical variables at once, or produce
#choose(4, 2) combinations, can we perform a transformation of the data to find the high-variance directions?

#-------------------------------------


#1) Firstly, standardise the iris_data - subtract each variable by its mean and divide by standard deviation
iris_data_std <- iris_data %>%
    mutate(SL_std = custom_standardise_func(Sepal.Length),
           SW_std = custom_standardise_func(Sepal.Width),
           PL_std = custom_standardise_func(Petal.Length),
           PW_std = custom_standardise_func(Petal.Width)) %>%
    select(contains("_std"))


#QA - check vars standardised, and has mean ~ 0 and sd ~ 1
mean(iris_data_std$SL_std)
sd(iris_data_std$SL_std)




#2) Now, from here, two methods of Acquiring the Eigenvalues and Eigenvectors

#-Method 1) Singular Value Decomposition (SVD) - prcomp
method_1_SVD <- prcomp(iris_data_std)
method_1_eigenvalues <- (method_1_SVD$sdev)^2
method_1_eigenvectors <- method_1_SVD$rotation





#-Method 2) Eigendecomposition of the Covariance Matrix
#First, compute the covariance matrix
cov_mat_iris <- cov(iris_data_std)


#Now perform eigendecomposition
method_2_eigendecomp_of_cov_mat <- eigen(cov_mat_iris)
method_2_eigenvalues <- method_2_eigendecomp_of_cov_mat$values 
method_2_eigenvectors <- method_2_eigendecomp_of_cov_mat$vectors


#And notice that the eigenvectors and eigenvalues are equivalent


#Check the % of variance explained by each PC:
eval_summary_df <- data.frame(princ_comp = seq(1, length(method_2_eigenvalues), by = 1), eval = method_2_eigenvalues) %>%
    as_tibble() %>%
    mutate(perc = 100.*eval/sum(eval),
           cumulative_perc = cumsum(perc))


scree_plot_pca <- ggplot(eval_summary_df, aes(x = princ_comp, y = perc)) +
    geom_line(size = 2) +
    geom_point(size = 5) +
    theme_bw() +
    xlab("Principal Component") +
    ylab("% of Variance Explained") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20))

ggsave("outputs/scree_plot.png", plot = scree_plot_pca)


#The first 2 eigenvectors explain ~96% of the variance - so re-projecting the data in terms of the first two principal components should capture what we need


#3) Re-projecting the original data in terms of the first two principal components 

#First, construct p' x p projection matrix:

V_p <- method_2_eigenvectors[,1:2]

#V_p is 4 x 2, original data is 150 x 4, so we want a 150 x 2 matrix as the result

projected_iris_data <- as.matrix(iris_data_std) %*% V_p %>%
    as_tibble() %>%
    rename(PC1 = V1,
           PC2 = V2) %>%
    bind_cols(iris_data %>% 
                select(Species))

#Remember that PC1 and PC2 here are also still in terms of the 'standardised' data
#We have also re-added the labels to help with interpreting the results/visualisation below


#Finally, plot projected data: 
reprojected_data <- ggplot(projected_iris_data, aes(x = PC1, y = PC2, colour = Species)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("PC1") +
    ylab("PC2") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20))

ggsave("outputs/pc1_vs_pc2_projected_data.png", plot = reprojected_data)

#I.e. we are representing our original (4D) data in a simpler 2D space
#From this, we basically see that:
#-Setosa is notably different from Versicolor and Virginica 
#-Versicolor and Virginica are less different from one another (but still somewhat different)
#-This supports the initial observations (scatter plots), so PCA applied to the Iris data doesn't actually tell us an awful lot
#-The real 'power' of PCA comes in when we perhaps have hundreds of variables (like e.g. when we have genetic data)




#To summarise PCA:
#1) PCA is a dimensional reduction technique used to transform a set of continuous, numerical variables into a smaller set of new, uncorrelated variables, whilst preserving information about the variability of the data 
#2) We achieve this by computing the eigenvalues and eigenvectors of the covariance matrix of the dataset. These identify the magnitude and direction of the dataset's variance, respectively. 
#3) Using the first two eigenvectors (principal components), we can re-project our original N-dimensional data in 2D, and plot it to observe which data points are inherently similar to one another. 
#4) PCA should thus be used as an exploratory data analysis tool which allows us to quickly identify if there are any natural groupings, patterns/clusters or underlying relationships in the data...
#...without needing to look at every pairwise combination of variables individually. 
#---BUT IN ITSELF IS NOT A CLUSTERING ALGORITHM



#------------------------------------------------
#Annex:

#Multi-Dimensional Scaling - implement via Cluster package

#Compute distance matrix d_ij - this is computing the Euclidean (or other) distance between every observation
#-E.g. diff between obs 1 and 2:
iris_data_1 <- iris_data[1,] %>%
    select(-Species)

iris_data_2 <- iris_data[2,] %>%
    select(-Species)

iris_diff_12 <- sqrt((iris_data_1$Sepal.Length - iris_data_2$Sepal.Length)^2 + 
                     (iris_data_1$Sepal.Width - iris_data_2$Sepal.Width)^2 +
                     (iris_data_1$Petal.Length - iris_data_2$Petal.Length)^2 +
                     (iris_data_1$Petal.Width - iris_data_2$Petal.Width)^2
)

#Same as:
iris_dist_12 <- iris_data[1:2,] %>%
    select(-Species) %>%
    dist()


#Apply to full data:
iris_dist <- iris_data %>%
    select(-Species) %>%
    dist()


#Do iris_dist %>% as.matrix(), and check nrow/ncol, observe that it is 150x150



#Now perform MDS analysis - works somewhat analogously to PCA, centering, performing eigendecomposition, selecting top k eigenvectors
#E.g. for when we want 2 dimensions, select the 2 eigenvectors with the largest eigenvalues
iris_mds <- cmdscale(iris_dist, k = 2) %>%
    as_tibble() %>%
    bind_cols(iris_data %>% select(Species))

iris_mds_plot <- ggplot(iris_mds, aes(x = V1, y = V2, color = Species)) +
    geom_point(size = 3) +
    theme_bw() +
    xlab("Dim 1") +
    ylab("Dim 2") +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 20))


ggsave("outputs/dim1_vs_dim2_iris_mds.png", plot = iris_mds_plot)

#The result is near-enough identical to PCA when we use a Euclidean distance metric
#Note that here we haven't rescaled the data prior to computing the distance matrix, which might explain why there's a few small differences. 
#But data is still measured on the same scale, I.e. all variables in cm, so that's why it's perhaps not that different


