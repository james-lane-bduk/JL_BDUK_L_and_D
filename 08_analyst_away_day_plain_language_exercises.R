#Load packages
library(dplyr)
library(ggplot2)
library(infer)
library(zoo)
library(lubridate)



#----Challenge 1: Linear regression on Iris data


#Select vars of interest from iris data
iris_data <- iris %>%
  as_tibble() %>%
  select(Sepal.Length, Petal.Length)


set.seed(1)
iris_data %>%
  slice_sample(n=10)


#Fit linear model to predict Petal Length from Sepal Length
iris_lm <- lm(Petal.Length ~ Sepal.Length, data = iris_data)

summary(iris_lm)
#Technical Frame: The coefficients from a linear regression of Petal Length on Sepal Length are -7.1 (intercept), and 1.9 (Gradient)
 

#Fitted values:
iris_data_add_preds <- iris_data %>%
  mutate(Petal.Length.Pred = predict(iris_lm))


#Plot along with fitted values
ggplot(iris_data_add_preds, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(colour = '#12346D', size = 2.0) +
  geom_line(aes(y = Petal.Length.Pred), colour = '#28A197', linewidth = 1.7) +
  theme_minimal() +
  xlab("Sepal Length (cm)") +
  ylab("Petal Length (cm)") +
  ggtitle("Sepal Lengths and Petal Lengths (in cm) for a range of Iris flowers") +
  theme(text = element_text(size = 14))

#Find RMSE
iris_data_add_preds %>% 
  mutate(.resid = Petal.Length - Petal.Length.Pred) %>% 
  mutate(.resid2 = (.resid)^2) %>% 
  summarise(mean(.resid2)) %>% 
  sqrt() %>%
  pull()

#Plain Language angles (see slides)
#1) Blue data points represent Iris flowers
#2) Green line is a straight best-fit line, which models overall trend
#3) Shows that as Sepal Length increases, Petal Length increases (for each 1cm Sepal Length increase, approx. 2cm Petal Length)
#4) We can use this green line to predict the Petal Length's of newly observed iris flowers, if we know their Sepal Length
#5) The model error is 0,76cm, meaning on average, any new predictions we make of Petal Length using Sepal Length will be 0.86cm away from true value

#--Frame in context of answering key questions - 
#1) What do we mean by data?
#2) What is 'linear regression?
#3) What do the results show?
#4) Why is this useful?
#5) What are the risks?

#-------------------------------------


#-----Challenge 2: P-values in the context of a Chi-Squared Significance Test on the Diamonds Dataset


#Select vars of interest from diamonds data
diamonds_data <- diamonds %>%
  as_tibble() %>%
  select(color, cut)

set.seed(1)
diamonds_data %>%
  slice_sample(n=3)


#Contingency table - compute number of diamonds by cut and colour
diamonds_by_colour_and_cut <- diamonds_data %>%
  group_by(color, cut) %>%
  summarise(n_diamonds = n()) %>%
  pivot_wider(names_from = "color", values_from = "n_diamonds")


#Look at distribution of diamonds by colour and cut, and compute percentage of diamonds by cut in each colour category to allow relative comparison
diamonds_by_colour_and_cut_add_pct <- diamonds_data %>%
  group_by(color, cut) %>%
  summarise(n_diamonds = n()) %>%
  mutate(pct_in_colour = round(100.*n_diamonds/sum(n_diamonds),1))

#Aggregate at colour-level so we can display no. diamonds in each colour category on chart
totals_by_colour <- diamonds_by_colour_and_cut_add_pct %>%
  summarise(n_diamonds = sum(n_diamonds))

#Create stacked bar plot - use gov. analysis function colour scheme, 5 cuts so 5 colours
my_cols <- c("Fair" = "#12436D",  "Good" = "#28A197", "Very Good" = "#801650", "Premium" = "#F46A25", "Ideal" = "#A285D1")

#Show distribution of diamond cuts within each colour category (% in each colour category adds up to 100%)
#Use accessible colours, labels, and overlay % onto bars
#Also show number of diamonds in each colour category for transparency/quantifying sample size
ggplot(diamonds_by_colour_and_cut_add_pct, aes(x = color, y = pct_in_colour, fill = cut)) +
  geom_col(position = "stack") +
  theme_minimal() +
  xlab("Diamond Colour") +
  ylab("% of Diamonds (in colour)") +
  ggtitle("Distribution of Diamonds, by Colour and Cut") +
  scale_fill_manual(values = my_cols) +
  geom_text(data = diamonds_by_colour_and_cut_add_pct %>% filter(pct_in_colour >= 5), aes(label = paste0(pct_in_colour, '%')), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 12)) +
  geom_text(data = totals_by_colour, aes(x = color, y = 103, fill = NULL, label = paste0(n_diamonds)), colour = 'red')



#Little variation in diamond cuts by colour overall, but let's perform a chi squared test of independence

#First, compute the chisq test statistic
chi_sq_calc <- diamonds_data %>%
  specify(color ~ cut) %>%
  calculate(stat = "chisq")

#Then, compute the test statistics we'd expect under the null hypothesis using permutation (1000 times)
set.seed(1)
chi_sq_sim <- diamonds_data %>%
  specify(color ~ cut) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "chisq")

#Plot the null distribution, the observed test statistic, and compute the p-value (fraction of times a null test stat > our test stat)

chi_sq_sim %>%
  visualize() +
  shade_p_value(obs_stat = chi_sq_calc, direction = "greater")


p_value <- chi_sq_sim %>%
  get_p_value(obs_stat = chi_sq_calc, direction = "greater")





#Plain Language angles (see slides)
#1) Each bar on chart - percentage breakdown of diamond cuts for a given colour
#2) Overall, little variation in diamond cuts by colour, suggesting no link, but there are some notable differences..
#...e.g. notable smaller proportion of J-coloured diamonds are of an ideal cut, compared to other colours
#3) A statistical significance test reveals that these observed differences would be extremely unlikely to occur by chance
#4) This suggests diamond cut and colour do have some bearing on one-another, despite the overall variation being small


#--Frame in context of answering key questions - 
#1) What do we mean by data?
#2) What do the results show?
#3) What further insights are there?
#4) What does this mean overall?



#-------------------------------------



#-----Challenge 3: AirPassenger trends over time - averages by month with standard deviation (across all years)

dates <- seq(from = as.Date("1949-01-01"), to = as.Date("1960-12-01"), by = "month")

air_passengers_df <- data.frame(Month = dates,
                                no_passengers = as.numeric(AirPassengers)) %>%
  as_tibble()



#Aggregate by month and compute mean no. of passengers, along with the sd to indicate variation
air_passengers_df_monthly <- air_passengers_df %>%
    mutate(month = month(dates, label = TRUE, abbr = TRUE)) %>%
  group_by(month) %>%
  summarise(avg_passengers = mean(no_passengers, na.rm=TRUE),
            sd_passengers = sd(no_passengers, na.rm = TRUE))


#Plot, and manually add labels for most and least common months
ggplot(air_passengers_df_monthly, aes(x = month, y = avg_passengers, group=1)) +
  geom_ribbon(aes(ymin = avg_passengers - sd_passengers, ymax = avg_passengers + sd_passengers), fill = "#F46A25", alpha = 0.3) +
  geom_line(aes(colour = 'Average'), linewidth = 1.8, alpha = 1.0) +
  geom_point(aes(colour = 'Average'), size = 4.0) +
  theme_minimal() +
  xlab("Month of Year") +
  ylab("No. of Passengers") +
  scale_colour_manual(values = c("Average" = "#12346D")) +
  labs(colour = "Key") +
  ggtitle("Average Volume of Airline Passengers by month of year, from 1949-1960") +
  geom_text(data = air_passengers_df_monthly %>% filter(month %in% c('Jul', 'Nov')), aes(x = month, y = avg_passengers + 20, label = paste0(round(avg_passengers,0)))) +
  theme(text = element_text(size = 12))


#Plain Language angles (see slides)
#1) The blue line on the chart shows the average number of passengers, by month, across all years.
#2) The orange region gives a rough indicator of the variability in the number of passengers for that given month across the years (1949 – 1960).
#3) We see that on average, the greatest number of passengers travelled in July (avg. of 351), closely followed by August - concurrent with school holidays
#4) On average, the fewest number of passengers travelled in November (avg. of 233) - more travel in December on avg. (Xmas holidays)



#--Frame in context of answering key questions - 
#1) What do we mean by data?
#2) What do the results show, and what is a possible narrative/explanation?
