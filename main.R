# Import packages
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

# Import data
data_voluntarios <- read_excel("Nueva Base.xlsx", sheet = "Principal", col_types = c("text",
                                                                           "date", "date", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "numeric")) %>% select(1:11) %>% select(-6)
summary(data_voluntarios)
View(data_voluntarios)

# Limpieza
# Tipos de datos
data_voluntarios_2 <- data_voluntarios
data_voluntarios_2$Origen <- as.factor(data_voluntarios_2$Origen)
data_voluntarios_2$`Tipo de Documento` <- as.factor(data_voluntarios_2$`Tipo de Documento`)
data_voluntarios_2$Nacionalidad <- factor(ifelse(data_voluntarios$Nacionalidad == "#N/A", NA, data_voluntarios$Nacionalidad))
data_voluntarios_2$`Horas Diarias` <- factor(as.integer(ifelse(grepl("[A-Za-z]", data_voluntarios$`Horas Diarias`), NA, data_voluntarios$`Horas Diarias`)))
data_voluntarios_2$`Dias Semanales` <- factor(as.integer(ifelse(grepl("[A-Za-z]", data_voluntarios$`Dias Semanales`), NA, data_voluntarios$`Dias Semanales`)))
data_voluntarios_2$Turno <- as.factor(data_voluntarios_2$Turno)
data_voluntarios_2 <- data_voluntarios_2[,-ncol(data_voluntarios)]

# Validaciones
fecha_edad_minima <- Sys.Date() - years(15)
data_voluntarios_2 <- data_voluntarios_2[data_voluntarios_2$`Fecha de Nacimiento` < fecha_edad_minima,]

data_voluntarios_2 <- data_voluntarios_2 %>%
  mutate(`Número de Documento NEW` = toupper(str_replace_all(`Número de Documento NEW`, "\\s", "")))

# Documentos Duplicados
#docs_duplicated <- data_voluntarios_2 %>% group_by(`Número de Documento NEW`) %>%  filter(n() > 1)

summary(data_voluntarios_2)
View(data_voluntarios_2)

