library(dplyr)
library(ggplot2)
library(patchwork)



#SECTION 1 - APPLYING THE EXPECTATION-MAXIMISATION ALGORITHM, AS USED BY GAUSSIAN MIXTURE MODELS (GMM) clustering
#-Purpose is to assign each data point a probability of belonging to a particular cluster




#Step 0) Generate synthetic data from 2 different Gaussian distributions:
set.seed(123)
n <- 300  #no. of data points

#150 data points from 1 distribution, 150 from the other.
x <- c(rnorm(n/2, mean = -2, sd = 1), rnorm(n/2, mean = 2, sd = 1))


#We want to find the parameters for our distributions that maximise the log likelihood
#-Initialise parameters (mixing proportion, mean and SD)
#--The mixing proportion is the fraction of the total data we expect to come from each distribution
pi1 <- 0.5
pi2 <- 0.5
mu1 <- -1
mu2 <- 1
sigma1 <- 1
sigma2 <- 3


#Apply EM algorithm - do 100 iterations
loglik <- c()
for (k in 1:100) {

       #Step 1) Expectation:

       #Equation 151 in paper - compute probability for each data point belonging to each cluster (1 or 2)
       gamma1 <- pi1 * dnorm(x, mean = mu1, sd = sigma1) / 
              (pi1 * dnorm(x, mean = mu1, sd = sigma1) + pi2 * dnorm(x, mean = mu2, sd = sigma2))

       gamma2 <- pi2 * dnorm(x, mean = mu2, sd = sigma2) / (pi1 * dnorm(x, mean = mu1, sd = sigma1) + pi2 * dnorm(x, mean = mu2, sd = sigma2))


       #Step 2) Maximisation (equations 154 in text)
       pi1 <- mean(gamma1)
       pi2 <- mean(gamma2)

       mu1 <- sum(gamma1 * x)/sum(gamma1)
       mu2 <- sum(gamma2 * x)/sum(gamma2)

       sigma1 <- sqrt(sum(gamma1 * (x - mu1)^2)/sum(gamma1))
       sigma2 <- sqrt(sum(gamma2 * (x - mu2)^2)/sum(gamma2))

       #Compute log-likelihood:
       loglik[k] <- sum(log(pi1 * dnorm(x, mean = mu1, sd = sigma1) + pi2 * dnorm(x, mean = mu2, sd = sigma2)))

       #Convergence check
       if (k > 1 && abs(loglik[k] - loglik[k - 1]) < 1e-6) break

}

#So I think gamma1 has the probabilities that data point n belongs to distribution 1...
#...and gamma2 has the probabilities that data point n belongs to distribution 2.
#I.e. this is what is returned in the 'z' variable when applying Mclust.


#NOTE - BEAR IN MIND THAT THIS EXAMPLE IS A BIT STRANGE AS WE'VE GENERATED SYNTHETIC DATA FROM 2 DISTRIBUTIONS...
#...AND THEN USED THAT SAME DATA TO UPDATE THE PARAMETERS OF THOSE DISTRIBUTIONS. IN PRACTICE, WE'D ASSUME...
#...THE DISTRIBUTIONS, THEN USE E.G. THE IRIS DATA TO UPDATE THE PARAMS WHICH MAXIMISE THE LIKELIHOOD & ASSIGN PROBS



#-----------------------------------




#Collate results
combined_data <- data.frame(orig_values = x, 
                            prob_distr1 = gamma1,
                            prob_distr2 = gamma2)


#-a) A histogram of the initial (full) distribution (x)
initial_data_histogram <- ggplot(combined_data, aes(x = x)) +
                            geom_histogram(binwidth = 0.2, fill = 'white', colour = '#12346D', linewidth = 2.0) +
                            theme_minimal() +
                            ylab("No. of data points") +
                            theme(text = element_text(size = 14),
                                axis.text = element_text(size = 14)) +
                            scale_x_continuous(limits = c(-5, 5))
                            


#-b) Original values vs probability they come from distr 1 and 2
value_vs_probability_plot <- ggplot(combined_data, aes(x = x)) +
                          geom_line(aes(y = prob_distr1, colour = "prob_distr1"), linewidth = 2.0) +
                          geom_line(aes(y = prob_distr2, colour = "prob_distr2"), linewidth = 2.0) +
                          theme_minimal() +
                          ylab("Probability") +
                          theme(text = element_text(size = 14),
                                axis.text = element_text(size = 14)) +
                          scale_x_continuous(limits = c(-5, 5)) +
                          scale_colour_manual(values = c("prob_distr1" = "#12346D", "prob_distr2" = "red"), 
                                              name = "Probability Curve")


#c) A line chart showing how the log-likelihood changes over time
loglik_results <- data.frame(iteration_no = seq(1, length(loglik), 1),
                             loglik = loglik)

loglik_results_plot <- ggplot(loglik_results, aes(x = iteration_no, y = loglik)) +
                        geom_line(colour = '#12346D', linewidth = 2.0) +
                        geom_point(colour = 'red', size = 3.0) +
                        theme_minimal() +
                        xlab("Iteration No.") +
                        ylab("Log Likelihood") +
                        theme(text = element_text(size = 14),
                                axis.text = element_text(size = 14))

all_EM_plots_combined <- initial_data_histogram / loglik_results_plot / value_vs_probability_plot
ggsave("outputs/all_EM_plots_combined.png", plot = all_EM_plots_combined)




#BELOW WAS GOING TO APPLY VARIATIONAL INFERENCE ALGORITHM (SIMILAR TO EM, ABOVE) TO APPLY VMFT
#-Purpose is to acquire a posterior distribution which approximates the underlying data
#BUT I HAVEN'T QUITE BEEN ABLE TO WORK OUT YET EXACTLY HOW THE IMPLEMENTATION/ALGORITHM IS SUPPOSED TO DIFFER

