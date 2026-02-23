install.packages("tidyverse")
library(tidyverse)

setwd("C:/Users/angel/Data-visualization")
getwd()
# Import the data
# By using the underscore in read_csv, I create a tibble file which is a more 
# enhanced version of a data frame in R
wholesale <- read_csv("Wholesale_customers_data.csv")
wholesale

# If you use dot in read.csv, you will import the dataset as an R dataframe
wholesale_df <- read.csv("Wholesale_customers_data.csv")
class(wholesale)
class(wholesale_df)



install.packages("dplyr")
library(dplyr)

wholesale |>
  glimpse(width = 70)

install.packages("skimr")
library(skimr)
skim(wholesale) # gives an overview of the data -> quick exploratory data analysis (EDA)




# Filter for rows, select for columns
# |> pipe operator. Passa il risultato di una funzione come primo argomento della funzione successiva
?select
wholesale |>
  filter(Channel == "Retail") # filter where Channel is equal to "Retail"

wholesale |> 
  select(starts_with("De"))

wholesale |>
  select(Channel, Region, Fresh, Grocery)

# Select every column but one
wholesale |> 
  select(-Region)

wholesale |> 
  select(starts_with("De"))

# Remove columns that contain "en"
wholesale |> select(-contains("en"))




# , AND: both conditions must be true
wholesale |>
  filter(Channel == "Retail", Fresh > 10000)

# | OR: at least one condition must be true
wholesale |>
  filter(Channel == "Retail" | Fresh > 50000)


# Arrange dataset according to values in a column
wholesale |>
  arrange(desc(Grocery)) |>
  select(Channel, Region, Grocery, Milk, Detergents_Paper)



# Adding or transforming columns within a dataset
# Add a new column called total_spend
total_spend <- wholesale |>
  mutate(
    total_spend = Fresh +
      Milk +
      Grocery +
      Frozen +
      Detergents_Paper +
      Delicassen
  )

wholesale

wholesale |>
  mutate(
    share_fresh = Fresh / total_spend,
    share_grocery = Grocery / total_spend
  ) |>
  select(Channel, share_fresh, share_grocery)

#Reduces data set and helpful for summary statistics
wholesale |>
  summarise(
    n = n(),  #how many 
    avg_total = mean(total_spend),
    med_total = median(total_spend),
    .by = Channel
  )

mean(wholesale$total_spend)
class(wholesale)

# Data exploration
View(wholesale) # or you can just point and click

# Your turn!
wholesale |> 
  mutate(
    share_grocery = Grocery/total_spend,
    share_fresh = Fresh/total_spend
  )

share_grocery
wholesale |> 
  mutate(
    share_grocery = Grocery/total_spend,
    share_fresh = Fresh/total_spend
  )
  filter(share_grocery > 0.3) |>
  arrange(desc(total_spend))


### Data Visualization

library(ggplot2)
data(mpg)
# tibble::glimpse(mpg)
?mpg
mpg

# Create the first plot
# 1. call to ggplot (it basically creates an empty plot)
# 2. we add geometries (identify the type of plot you wanna construct -> geom_point for scatter plots)
# 3. what are the variables we want to plot, in the aestethic call
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) 
# we notice that as the displayment increases, the comsumption decreases

# It can create a legend to the differemt colors associated to the different values in the class column
# This way we can also point out the outliers and what they are
?geom_point
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) 

# This is a hard to read plot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = drv, shape = drv))
 
# We convey one extra layer of information in respect to the previous one
# It's also more readable
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class, shape = drv))

?geom_boxplot

# It uses a weighted moving mean, which is the gray part and it's the estimated variability around the point
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
# To remove the gray part, we do this:
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv), se = FALSE)

# Represent two geometries on the same plot -> to the smooth line, I add the scatter plot
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x = displ, y = hwy))
