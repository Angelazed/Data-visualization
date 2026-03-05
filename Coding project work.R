# Import dataset & libraries::
weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-12-20/cities.csv')
outlook_meanings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-12-20/outlook_meanings.csv')

library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
"""
1. Apply data manipulation and visualization techniques learned in class
2. Extract meaningful insights through both statistical summaries and visual exploration
3. Present you finding in a clear report

Data wrangling with dplyr and the tidyverse
Data visualization with ggplot2
Reproducible reporting with Quarto
Critical thinking about data and visual communication

1 - Two insights using data manipulation (dplyr)
Statistical summaries, grouping, filtering, transformations
Show your ability to wrangle and understand the data
2 - Two insights using data visualization (ggplot2)
Create meaningful, well-designed visualizations
Demonstrate mastery of the grammar of graphics

An insight is a meaningful finding that helps understand the data:

A pattern or trend discovered through analysis
A comparison between groups or variables
A surprising or non-obvious relationship
A clear answer to a specific question about the data
"""

"""
Exploratory Data Analysis
"""

weather_forecasts |>
  glimpse(width = 70)

skim(weather_forecasts)

weather_forecasts |>
  filter(!is.na(observed_precip)) |>
  summarise(mean_precip = mean(observed_precip))  

library(dplyr)

weather_forecasts |>
  filter(!is.na(observed_temp)) |>
  summarise(mean_precip = mean(observed_temp))   

outlook_meanings |>
  glimpse(width = 70)

cities |>
  glimpse(width = 70)

#Clean dataset
weather_forecasts_cleaned <- weather_forecasts |>
  filter(!is.na(observed_precip)) |>
  filter(!is.na(observed_temp))
# In total were removed around 50000 rows that presented missing values either 
# in observed_precip or observed_temp. This is around 20% of the original dataset.

# Convert observed_temp from Fahrenheit to Celsius
weather_forecasts_cleaned <- weather_forecasts_cleaned |>
  mutate(observed_temp_c = (observed_temp - 32) * 5 / 9)

"""
Data Visualization
"""