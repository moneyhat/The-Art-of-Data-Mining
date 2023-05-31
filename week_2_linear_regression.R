#################### BACKGROUND ####################

# This week's challenge revolves around understanding how weather conditions influence the number of bike rides in a given day. This is real 2018 data from Chicago's DIVVY bikeshare system.  

# you have the following relevant columns:
# number_rides - number of rides in a given day
# AWND - Average wind speed in the day
# TAVG - Average temperature in Fahrenheit
# SNOW - Total snowfall in the day
# PRCP - Total rainfall in the day

# load libraries 
library(tidyverse)

# load data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1DK8ZSmIgvZ1eVVF33NCNLyLxvYFA8t1jeGrJppaBDpI/gviz/tq?tqx=out:csv")

############## Linear regression

# QUESTION -  fit a regular regression model using TAVG as a feature to explain your target, number_rides.

# ANSWER -
bikes_model_TAVG <- lm(number_rides ~ avg_temp, data = bikes)
bikes_model_TAVG_summary <- summary(bikes_model_TAVG)

############################################################################
# QUESTION - How much variation is being explained in your target? Please use code to get the answer from the summary and not just write out the answer

bikes_model_TAVG_summary$r.squared
# ANSWER - 78.59% of the variation can be explained by this modelk

############################################################################
# QUESTION - Calculate the confidence interval for B1 - Do it in a way that works if model structure or data gets added to or removed!

bikes_model_TAVG_summary$coefficients[2,1] + bikes_model_TAVG_summary$coefficients[2,2]
# ANSWER -[1] 258.0827
###########################################################################

# QUESTION - Interpret your B1 coefficient in 'plain English.'

bikes_model_TAVG_summary$coefficients[2,1]
# ANSWER - For every one unit increase increase in Average Temperature ß1 there is a 258.0827 increase in the number of rides

############################################################################
# QUESTION - Calculate the predicted number of rides if the average temperature is 66 degrees

# ANSWER - 
############################################################################
# QUESTION - Make a figure showing the relationship between TAVG and number_rides. This is two continuous variables which should tell you what type of plot you need.  You can then make a vector of x-values.  Then extract the coefficients from the model to predict y.  You then have all you need to add a geom_line() to your plot!

# ANSWER - 
ggplot(bikes_model_TAVG,
       aes(x = number_rides, y = avg_temp)) +
  geom_point() +
  theme_minimal() +
  labs(x = 'Number of Rides')+
  labs(y = 'Average Temperature (ºF)')+
  geom_smooth(method = 'lm')


############## Comparing two linear regression models ############################
# QUESTION - Fit another regression model using avg_wind as a feature but leave number_rides as the target

# ANSWER - 
AWND_model <- lm(number_rides ~ avg_wind, data = bikes)
summary_AWND_model <- summary(AWND_model)

############################################################################
# QUESTION - Which is a better model, this one or the first one you fit?  Use two pieces of evidence from the model summaries to justify your answer.

summary_AWND_model$r.squared
summary_AWND_model$fstatistic
# ANSWER - although significant, 9.5% of the variance can be accounted for by this model thus, the TAVG model is a better fit.


############## Multiple regression ################################################

# QUESTION -  fit a multiple regression model with number of rides as the target and then AWND, PRCP, SNOW, and TAVG as features

# ANSWER - 
multiple_r_bikes <- lm(number_rides ~ avg_wind + precipitation + snow + avg_temp, data = bikes)
summary_multiple_r_bikes <- summary(multiple_r_bikes)

############################################################################
# QUESTION - How much extra variation did you explain by including these other features? 
extra_var <- summary_multiple_r_bikes$r.squared - summary_AWND_model$r.squared

extra_var
# ANSWER -  we were able to explain roughly 75.86 more of the variation by including these features
############################################################################
# QUESTION - Were any of the additional parameters not important?  Use two pieces of evidence to justify your answer.

summary_multiple_r_bikes

# ANSWER - it appears that Snow was an insignificant factor in this model with a p-value of just .899 which means we can only be roughly 10% certain that Snow has an affect on this model.

############## Multiple regression - interaction models ##########################
# finally, we're going to fit an interaction model
# before that, run this line of code that creates a binary snow feature so that it's just a 1 if there's any snow that day, and a zero if there's not

bikes$SNOW_B <- as.factor(ifelse(bikes$snow > 0, 1, 0))

############################################################################
# QUESTION - fit an interaction model between TAVG and SNOW_B

# ANSWER - 
bikes_interaction <- lm(number_rides ~ avg_temp * snow, data = bikes)

############################################################################
# QUESTION - Interaction models are hard to interpret, so make a plot with two fit lines instead.  Remember, make a sequence of X values, then use these to estimate the Y values... one Y vector for if it snowed, the other if it didn't.

# ANSWER -

bikes_n <- nrow(bikes) #nrow() gets the number of rows in a dataframe

# The function seq() generates sequences in a range of values
# You provide the starting value of the sequence
# Then the max value
# Then how long you want the resulting sequence to be - in this case the length of the data frame
bikes_seq <- seq(min(bikes$avg_temp), max(bikes$avg_temp), length.out = bikes_n)

#punching  this range of x values into our formulas. 

int_model <- summary(bikes_interaction) #store our model summary
int_model$coefficients # 

y_TAVG <- int_model$coefficients[1,1] + int_model$coefficients[2,1] * bikes_seq + 
  int_model$coefficients[3,1] * 1 + 
  int_model$coefficients[4,1] * bikes_seq * 1

y_SNOW <- int_model$coefficients[1,1] + int_model$coefficients[2,1] * bikes_seq + int_model$coefficients[3,1] * 0 + int_model$coefficients[4,1] * bikes_seq * 0

ggplot(bikes, aes(x = number_rides, y = avg_temp)) +
  xlab('Number of Riders') +
  ylab('Average Temperature (F)') +
  theme_minimal() +
  geom_point(aes(color = SNOW_B)) +
  geom_line(aes(x = y_TAVG, y = bikes_seq)) +
  geom_line(aes(x = y_SNOW, y = bikes_seq))


############################################################################
# QUESTION -  Based on the plot you created, interpret how snow and temperature interact to influence the number of rides. In other words, how does it snowing or not influence the relationship between temperature and the number of rides in a day?

# ANSWER - this model indicates that snow doesn't factor much into ridership nearly as much as temperature does. They are still clustered into the group of riders who are riding even when there ISN'T snow out.

