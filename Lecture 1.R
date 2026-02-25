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
wholesale <- wholesale |>
  mutate(
    total_spend = Fresh +
      Milk +
      Grocery +
      Frozen +
      Detergents_Paper +
      Delicassen
  )


wholesale |>
  select(total_spend)

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

## Your turn!
# EX.1
wholesale1 <- wholesale |> 
  mutate(
    share_grocery = Grocery/total_spend,
    share_fresh = Fresh/total_spend
  )

wholesale1 |>
  select(share_grocery)

wholesale1 |>
  filter(share_grocery > 0.3) |>
  arrange(desc(total_spend))


# EX.2

wholesale1 |>
  group_by(Channel) |>
  summarise(mean(Fresh), median(Fresh), sd(Fresh))


# EX.3
?slice_max
wholesale1 |>
  group_by(Region) |>
  slice_max(order_by = Milk,n = 5)


#EX:4
wholesale |>
  mutate(high_spender = total_spend > median(total_spend)) |>
  group_by(Channel) |>
  summarise(n_high_spenders = sum(high_spender))


"""
Data Visualization
"""

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

## Your turn!
# EX.1
ggplot(data = mpg) +
  geom_point(mapping = aes(cty, hwy, color = class, size = displ))


# EX.2
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(class, hwy))


# EX.3
ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = hwy, color = drv), bins = 20)
# bins is outside aes() because it's not a variable, it's an argument of geom_histogram

# EX.4
ggplot(data = mpg) +
  geom_bar(mapping = aes(x=manufacturer, fill=class), position = "dodge")
# fill is to color bars for each class, and dodge makes them one next to another instead of stacking

# EX.5
ggplot(data = mpg) +
  geom_density(mapping = aes(x = cty, color = drv), alpha = 0.2)
# alpha controls the transparency of point, lines, etc.. 0.5 means 50% transparency


"""
Patchwork can combine different charts in a single graphic. NOTE: it prioritizes parenthesis
"""
gg_bar <- ggplot(mpg) +
  geom_bar(aes(x = drv)) +
  theme_bw()

gg_bubble <- ggplot(data = mpg) +
  geom_point(
    mapping = aes(
      x = displ,
      y = hwy,
      size = cyl,
      color = as.factor(cyl)
    ),
    alpha = 0.7
  ) +
  scale_color_viridis_d(option = "C", name = "Cylinders") +
  scale_size_continuous(name = "Cylinders") +
  theme_bw()


library(patchwork)
gg_bar +
  gg_bubble  

gg_box <- ggplot(mpg) +
  geom_boxplot(aes(x = hwy, y = class, fill = class), alpha = 0.8)
(gg_bar + gg_box) / gg_bubble

"""
Time-series data use readxl for plotting
"""

library(readxl)
stock <- read_excel("stock.xlsx")
ggplot(stock, aes(y = EU, x = time)) +
  geom_line() +
  xlab("time") +
  theme_bw()


install.packages("ggcorrplot")
library(ggcorrplot)
# I created a new tibble removing the time column (temporal variable)
stock_data <- stock[, -1] 
class(stock_data)
# cor() calculates pairwise correlations between all numeric columns
# Output = a square matrix
corr <- cor(stock_data)
# Rounds the correlation matrix to 2 decimal places for readability
round(corr, 2)

ggcorrplot(corr)


ggcorrplot(
  corr,
  hc.order = TRUE,  #This reorders the variables using hierarchical clustering.
  outline.col = "white",  #Adds white borders around the squares in the heatmap
  ggtheme = ggplot2::theme_bw,  #Applies the theme_bw() theme from ggplot2
  colors = c("lightblue", "white", "pink") #Defines the color scale for negative, zero and
                                          #positive correlations
  )


"""
Visualizing maps and geospatial data using maps package
"""
install.packages("maps")
library(maps)
worldcities <- read_csv("worldcities.csv")

# map_data function converts maps to data frames for ggplot2
ita <- map_data("italy")
head(ita, 5)


gg_ita_map <- ggplot(data = ita) +
  geom_polygon(  #This layer draws polygons, which are used to create maps
    aes(x = long, y = lat, group = group),
    col = "dodgerblue4",  #border color of the map
    fill = "lightgray",  #inside color of the regions
    linewidth = 0.2  #thin border lines 
  ) +
  coord_fixed(1) +
  theme_bw()

# coord_fixed (1) = this fixes the aspect ratio -> IMPORTANT FOR GEO DATA
# Ensures that 1 unit on the x-axis equals 1 unit on the y-axis

gg_ita_map


# Filter for Italy
ita_cities <- worldcities |>
  filter(country == "Italy")

# Selects the 300 cities with the highest population
biggest_ita_cities <- ita_cities |>
  slice_max(order_by = population, n = 300)

# Take the base Italy map you created earlier (gg_ita_map) and add another layer with +
gg_ita_map +
  geom_point(
    data = biggest_ita_cities,
    aes(x = lng, y = lat, col = log(population), size = population),
    alpha = 0.7
  ) +
  scale_color_viridis_c()
# scale_color_viridis_c = applies a continuous viridis color scale ->
# designed to be perceptually uniform, colorblind-friendly and good contrast