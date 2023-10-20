# Import packages
library(readxl)
library(dplyr)

# Import data
volunteers_data <- read_excel("volunteers.xlsx", sheet = "Principal") %>%
  select(1:11) %>% select(-6)
str(volunteers_data)
