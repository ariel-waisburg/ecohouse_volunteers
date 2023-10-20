# Import packages
library(readxl)
library(dplyr)

# Import data
datavoluntarios <- read_excel("Nueva Base.xlsx", sheet = "Principal", col_types = c("text",
                                                                           "date", "date", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "numeric")) %>% select(1:11) %>% select(-6)
str(datavoluntarios)
View(datavoluntarios)
