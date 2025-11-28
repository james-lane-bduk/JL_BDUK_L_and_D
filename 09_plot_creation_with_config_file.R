library(dplyr)
library(ggplot2)


#Load Iris Data
iris_data <- iris %>%
  as_tibble()

#Get config file (looks for config.yml at working directory by default)
Sys.setenv(R_CONFIG_ACTIVE = "production")
cfg <- config::get()


iris_data_filtered <- iris_data %>%
  filter(Species == cfg$species_filter)


ggplot(iris_data_filtered, aes(x = !!sym(cfg$x_var), y = !!sym(cfg$y_var))) +
  geom_point(colour = '#12346D', size = 2.0) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  xlab(paste0(gsub("\\.", " ", cfg$x_var), ' (cm)')) +
  ylab(paste0(gsub("\\.", " ", cfg$y_var), ' (cm)')) +
  ggtitle(paste0("A scatter plot of ", gsub("\\.", " ", cfg$x_var), ' vs ', gsub("\\.", " ", cfg$y_var), ' for a range of ',cfg$species_filter, ' flowers'))

