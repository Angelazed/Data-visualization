install.packages("tidyverse")
library(tidyverse)

# Import the data
wholesale <- read.csv("Wholesale_customers_data.csv")
wholesale

install.packages("dplyr")
library(dplyr)

wholesale |> 
  select(starts_with("De"))

wholesale |>
  select(Channel, Region, Fresh, Grocery)

# Select every column but one
wholesale |> 
  select(-Region)

wholesale |> 
  select(starts_with("De"))

#,,,,,
wholesale |> select(-contains("en"))

install.packages("skimr")
library(skimr)
skim(wholesale)