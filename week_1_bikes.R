# For this challenge we're going to do some simple exploration and manipulation of the 2018 Chicago DIVVY bikeshare data.  Each row represents a single bike rental. 

library(tidyverse)

bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")



#############################################################################
# QUESTION - Use your data exploration techniques to look at both a summary and glimpse of bikes.  Write a brief response as well describing what's contained a couple numeric and character columns.  How many rows and variables are present?

summary(bikes)
glimpse(bikes)

# Answer: It appears we have trip times and information, unique id's for each bike, whether the customer was a subscriber or not, demographic information regarding the customer

#############################################################################
# QUESTION - How many unique values are present in the column from_station_id?
unique(bikes$from_station_id)

# Answer:  There are 12 unique values present in the "from_station_id" column.

#############################################################################
# QUESTION - Use some visual exploratory data analysis by making histograms of two of the numeric columns (your choice).  Do one in base R and one using ggplot.


hist(bikes$birthyear)

ggplot(bikes,
       aes(x = distance_miles)) +
  geom_histogram(breaks=seq(8))

#############################################################################
# QUESTION - Use base R to slice the data frame several different ways. Specifically, to do the following operations: Grab just a single row, a single column, a series of rows, a series of columns, a list of rows, and a list of columns. 

bikes[1,] #grabbing a single row
bikes[,5] #grabbing a single column
bikes[2,] #grabbing columns 2-10
bikes[7:0,] #grabbing a series of rows
bikes[0:5,] #grabbing a series of columns

#this question was a little confusing for me, but was able to manipulate, inspect most of the data present in each row/column

#############################################################################
# QUESTION - Use base R to extract just the columns usertype, gender, and distance_miles to a dataframe called bikes_1.  Do this again using tidyverse.


bikes1 <- c(bikes$gender, bikes$usertype, bikes$distance_miles) 
#now using vectors to create a new dataframe
#not sure if i missed something in base R but this is my best guess for now
bikes1 <- bikes[8:12]
unique(bikes1)

#using tidyVerse to make columns by userType, gender, and distance
bikes1<- bikes%>%
  select(gender, usertype, distance_miles)

unique(bikes1)  


  
#############################################################################  
#QUESTION - Use base R to create a dataframe of just subscribers.  Call the resulting dataframe bikes_subs.  Do this again using tidyverse

bikes_subs <- bikes[bikes$usertype == "Subscriber", ]

#using tidyVerse to select userType column and filter to grab rows meeting subscriber
bikes_subs <- bikes%>%
  select(usertype)%>%
  filter(bikes$usertype == "Subscriber")

bikes_subs

#############################################################################
# QUESTION - Use tidyverse to create a dataframe of just Subscribers who are also Female. Name this data frame df_subs_f

df_subs_f <- bikes %>% 
  select(gender) %>% 
  filter(bikes$gender == "Female") %>% 
  na.omit() 

#created a new dataframe
#selected the gender column
#filtered the rows by specified criteria
#Omitted the NA values to tidy up the data

#############################################################################
# QUESTION - What's the average distance ridden by female subscribers?

df_subs_f <- bikes %>% 
  select(gender, distance_miles) %>% 
  filter(bikes$gender == "Female") %>% 
  na.omit() 

mean(df_subs_f$distance_miles)
#Answer: The average distance ridden by female riders was: 1.347448 miles

#############################################################################
# QUESTION - Birthyear isn't super useful right now.  Having actual rider age would be better.  Use either base R or tidyverse to create a new column that calculates the rider's age (these data were collected in 2018, btw).  After it's made explore your new column (using whatever method you'd like) to make sure it makes sense.

bikes <- bikes%>%
  mutate(rider_age = 2018 - birthyear)

glimpse(bikes$rider_age)
#used the mutate function to create a new column called rider_age, subtracting the birthyear from the year 2018 to define age.

#############################################################################
# QUESTION - I'm guessing you see some strange values in your newly created age column.  Why don't you create a new dataframe called bikes_realages where it only contains riders who ages are less than or equal to some age that seems realistic to you.

bikes <- bikes%>% 
  mutate(rider_age = 2018 - birthyear) %>%
  filter(rider_age <= 80 | is.na(rider_age))

glimpse(bikes)
#############################################################################
# QUESTION - Make a histogram of rider ages using ggplot.  Based on this, what age range used the bikeshare the most?

ggplot(bikes,
       aes(x=rider_age)) +
  geom_histogram(breaks=seq(20,50, by = 2), col="black", fill = "blue") +
  xlim(c(18,55)) 

#Answer: The 24-30 age range has the most frequency in ridership 

#############################################################################
# QUESTION - Some of these data types could or need to be changed. There are three variables that are currently numeric but should be a factor. What are they and why?  After identifying them please convert them in the existing bikes data frame. 

#Answer: from_station_id, to_station_id, bike_id

bikes$tourist <-as.factor(bikes$from_station_id)
bikes$usertype <- as.factor(bikes$to_station_id)
bikes$bikeid <- as.factor(bikes$bikeid)

#############################################################################
# QUESTION - What was the most frequently used bike?

summary(bikes$bikeid)

#Answer: Bike 6234 appears to be the villiage bicycle with 74 rides

#############################################################################
# QUESTION - How many miles in total was the most frequently used bike ridden.  You're going to need to filter and then do a sum on a column...


bikes_6234 <- bikes %>%
  filter(bikes$bikeid == 6234) #skipping and removing na values from the calculation

glimpse(bikes_6234)


sum(bikes_6234$distance_miles, na.rm = TRUE) #summing up the distance, in miles, for the column

#Answer: Bike 6234 had a total of 83.31822 miles ridden

#############################################################################

