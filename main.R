# Import packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(mice)
library(naniar)

# Import data ----------------------------------------------------------------
data_voluntarios <- read_excel("Nueva Base (2).xlsx", sheet = "Principal", col_types = c("text",
                                                                           "date", "date", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "numeric")) %>% select(1:11) %>% select(-6)
summary(data_voluntarios)
View(data_voluntarios)

# Limpieza ----------------------------------------------------------------

# Tipos de datos
data_voluntarios_2 <- data_voluntarios
data_voluntarios_2$Origen <- as.factor(data_voluntarios_2$Origen)
data_voluntarios_2$`Tipo de Documento` <- as.factor(data_voluntarios_2$`Tipo de Documento`)
data_voluntarios_2$Nacionalidad <- factor(ifelse(data_voluntarios$Nacionalidad == "#N/A", NA, data_voluntarios$Nacionalidad))
data_voluntarios_2$`Horas Diarias` <- factor(as.integer(ifelse(grepl("[A-Za-z]", data_voluntarios$`Horas Diarias`), NA, data_voluntarios$`Horas Diarias`)))
data_voluntarios_2$`Dias Semanales` <- factor(as.integer(ifelse(grepl("[A-Za-z]", data_voluntarios$`Dias Semanales`), NA, data_voluntarios$`Dias Semanales`)))
data_voluntarios_2$Turno <- as.factor(data_voluntarios_2$Turno)
data_voluntarios_2 <- data_voluntarios_2[,-ncol(data_voluntarios)]
View(data_voluntarios_2)


# Validaciones
fecha_edad_minima <- Sys.Date() - years(15)
data_voluntarios_2 <- data_voluntarios_2[data_voluntarios_2$`Fecha de Nacimiento` < fecha_edad_minima,]

data_voluntarios_2 <- data_voluntarios_2 %>%
  mutate(`Número de Documento NEW` = toupper(str_replace_all(`Número de Documento NEW`, "\\s", "")))

# Documentos Duplicados
data_voluntarios_2 %>%
  group_by(`Número de Documento NEW`) %>%
  filter(n() > 1) %>%
  arrange(`Número de Documento NEW`)
data_voluntarios_3 <- data_voluntarios_2[!duplicated(data_voluntarios_2[,-c(1,2,9)]),]
data_voluntarios_3 <- data_voluntarios_3[complete.cases(data_voluntarios_3$`Número de Documento NEW`),]
check_dnis_unique <- length(unique(data_voluntarios_2$`Número de Documento NEW`)) == nrow(data_voluntarios_3)

summary(data_voluntarios_3)
View(data_voluntarios_3)

# Outliers ----------------------------------------------------------------

# Univariados

# 1. Analizar Marca Temporal
# 2. Analizar Fecha de Nacimiento / Edad

# Multivariados

# Missings ----------------------------------------------------------------
md.pattern(data_voluntarios_3, rotate.names = TRUE)

cor(data_voluntarios_3 %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric), use = "pairwise.complete.obs", method = c("pearson")) %>% round(digits = 2)
cor(data_voluntarios_3 %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric), use = "complete.obs", method = c("pearson")) %>% round(digits = 2)
cor(data_voluntarios_3 %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric), use = "complete.obs", method = c("spearman")) %>% round(digits = 2)

dummyNA <-
  as.data.frame(abs(is.na(data_voluntarios_3))) %>%
  select(Nacionalidad, `Horas Diarias`, `Dias Semanales`, `Turno`, `Marca Temporal`)

cor(dummyNA) %>% round(digits = 2)

cor(data_voluntarios_3 %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric),
    dummyNA, use = "pairwise.complete.obs") %>%
  round(digits = 2)

# De faltantes:
# * Marca Temporal es "inimputable"
# * Dias, Turno y Horas faltan la mitad de los registros como para imputarlos   => preguntar si sirve imputar igual.
# * Nacionalidad tiene características más imputables.

# Tests para ver si hay diferencias significativas entre los valores ausentes y no ausentes.

# De Nacionalidad
mcar_test(data_voluntarios_3 %>% select(`Nacionalidad`, `Número de Documento NEW`)) # NO sign.
mcar_test(data_voluntarios_3 %>% select(`Nacionalidad`, `Horas Diarias`))
mcar_test(data_voluntarios_3 %>% select(`Nacionalidad`, `Dias Semanales`))

# Horas Diarias
mcar_test(data_voluntarios_3 %>% select(`Marca Temporal`, `Horas Diarias`)) # NO sign.
mcar_test(data_voluntarios_3 %>% select(`Tipo de Documento`, `Horas Diarias`))
mcar_test(data_voluntarios_3 %>% select(`Turno`, `Horas Diarias`)) # NO sign.

# Dias Semanales
mcar_test(data_voluntarios_3 %>% select(`Marca Temporal`, `Dias Semanales`)) # NO sign.
mcar_test(data_voluntarios_3 %>% select(`Tipo de Documento`, `Dias Semanales`))

# Dias Semanales / Turno
mcar_test(data_voluntarios_3 %>% select(`Turno`, `Dias Semanales`))

# Resto
mcar_test(data_voluntarios_3 %>% select(`Fecha de Nacimiento`, `Número de Documento NEW`))

# Faltan aplicar la imputacion

# Proporcion de vacios de los campos Turno, Horas diarias y Dias Semanales en eventuales
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$Turno))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$`Horas Diarias`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$`Dias Semanales`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$`Marca Temporal`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
#
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",]$Turno))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",]$`Horas Diarias`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",]$`Dias Semanales`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",]$`Marca Temporal`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",])
#
#
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$Turno))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$`Horas Diarias`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$`Dias Semanales`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])
# sum(is.na(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$`Marca Temporal`))/nrow(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",])

sum(is.na(data_voluntarios_3$Turno))/nrow(data_voluntarios_3)
sum(is.na(data_voluntarios_3$`Horas Diarias`))/nrow(data_voluntarios_3)
sum(is.na(data_voluntarios_3$`Dias Semanales`))/nrow(data_voluntarios_3)
sum(is.na(data_voluntarios_3$`Marca Temporal`))/nrow(data_voluntarios_3)

