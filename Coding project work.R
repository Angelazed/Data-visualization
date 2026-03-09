# Import dataset & libraries::
weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-12-20/cities.csv')

library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(ggcorrplot)
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

weather_forecasts |>
  filter(!is.na(observed_temp)) |>
  summarise(mean_precip = mean(observed_temp))   

cities |>
  glimpse(width = 70)

#Clean dataset
weather_forecasts_cleaned <- weather_forecasts |>
  filter(!is.na(forecast_temp)) |>
  filter(!is.na(observed_precip)) |>
  filter(!is.na(observed_temp)) 
  
cities_cleaned <- cities |>
  filter(!is.na(wind))

# In total were removed around 77,000 rows that presented missing values. 
# This is around 12% of the original dataset.

# Convert observed_temp from Fahrenheit to Celsius
weather_forecasts_cleaned <- weather_forecasts_cleaned |>
  mutate(observed_temp_c = (observed_temp - 32) * 5 / 9)

weather_forecasts_cleaned <- weather_forecasts_cleaned |>
  mutate(forecast_temp_c = (forecast_temp - 32) * 5 / 9)

weather_forecast_accuracy <- merge(weather_forecasts_cleaned, cities_cleaned, by = "city")
"""
Data Manipulation with Dplyr
"""
#Forecast horizon vs. error
weather_forecast_accuracy|>
  group_by(forecast_hours_before) |>
  summarise(n= n(),
    mean_error = mean(forecast_temp_c-observed_temp_c),
    median_error = median(forecast_temp_c-observed_temp_c),
    mean_sd = sd(forecast_temp_c-observed_temp_c))
  
#(Since the mean absolute error is negative, it means that on average the forecast was too cold )
#(As we can see, the standard deviation increases as the hours before the observation increase )

#Are some climate zones harder to predict than others?
summary_koppen_error <- weather_forecast_accuracy|>
  group_by(koppen) |>
  summarise(n = n(),
    mean_error = mean(forecast_temp_c-observed_temp_c),
    median_error = median(forecast_temp_c-observed_temp_c),
    mean_sd = sd(forecast_temp_c-observed_temp_c)) |>
  filter(n > 10000) |>
  arrange(mean_error) 


 summary_koppen_error |> 
   ggplot(aes(x = reorder(koppen, mean_error), y = mean_error)) +
  geom_col(fill = 'light blue') +
   labs(
    x = "Köppen climate",
    y = "Mean forecast error (°C)",
    title = "Weather forecast mean error by climate type"
  )

#(Since in the data there are 15 climates included and some presente more data than others, 
# we decided to study only the top 7)
#(The hardest type of climate to forecast is the Dfc Subarctic climate, which has
# no significant difference in precipitations between seasons)

"""
Data Visualition with ggplot2
"""

#Do cities closer to the coast have higher forecast errors?

