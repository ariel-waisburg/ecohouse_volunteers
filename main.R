# Import packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(mice)
library(naniar)
library(ggplot2)


# Import data ----------------------------------------------------------------
data_voluntarios <- read_excel("Nueva Base (3).xlsx", sheet = "Principal", col_types = c("text",
                                                                           "date", "date", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "numeric")) %>% select(1:11) %>% select(-6)

# Limpieza ----------------------------------------------------------------

# data_voluntarios = data_voluntarios %>% mutate(edad = year(Sys.Date()) - year(`Fecha de Nacimiento`))
# outliersedad = data_voluntarios$edad[data_voluntarios$edad > median(data_voluntarios$edad,na.rm = T) + 1.5*IQR(data_voluntarios$edad,na.rm = T) | data_voluntarios$edad < median(data_voluntarios$edad,na.rm = T) - 1.5*IQR(data_voluntarios$edad,na.rm = T)]
# length(na.omit(outliersedad))

data_voluntarios <- data_voluntarios %>%
  mutate(`Dias Semanales` = ifelse(grepl("^Días de semana por la tarde", `Disponibilidad Horaria`), 4, `Dias Semanales`),
         `Horas Diarias` = ifelse(grepl("^Días de semana por la tarde", `Disponibilidad Horaria`), 3, `Horas Diarias`),
         Turno = ifelse(grepl("^Días de semana por la tarde", `Disponibilidad Horaria`), "TARDE", Turno))

data_voluntarios <- data_voluntarios %>%
  mutate(`Dias Semanales` = ifelse(grepl("^Fines de semana y feriados", `Disponibilidad Horaria`), 2, `Dias Semanales`),
         `Horas Diarias` = ifelse(grepl("^Fines de semana y feriados", `Disponibilidad Horaria`), 3, `Horas Diarias`),
         Turno = ifelse(grepl("^Fines de semana y feriados", `Disponibilidad Horaria`), NA, Turno))

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
orden_permanencia = c("Recibidos","Actuales","Eventuales","Formulario", "Historial")
data_voluntarios_2$Origen <- factor(data_voluntarios_2$Origen, levels = orden_permanencia)
data_voluntarios_2 <- data_voluntarios_2[order(data_voluntarios_2$Origen), ]
data_voluntarios_3 <- data_voluntarios_2[!duplicated(data_voluntarios_2[,-c(1,2,9)]),]
data_voluntarios_3 <- data_voluntarios_3[complete.cases(data_voluntarios_3$`Número de Documento NEW`),]
check_dnis_unique <- length(unique(data_voluntarios_3$`Número de Documento NEW`)) == nrow(data_voluntarios_3)

data_voluntarios_3[duplicated(data_voluntarios_3$`Número de Documento NEW`),]

summary(data_voluntarios_3)
View(data_voluntarios_3)
table(data_voluntarios_3$Origen) / length(data_voluntarios_3$Origen) * 100 # para ver las freq. relativas

# Outliers ----------------------------------------------------------------

data_voluntarios_3 = data_voluntarios_3 %>% mutate(edad = year(Sys.Date()) - year(`Fecha de Nacimiento`))
outliersedad = data_voluntarios_3$edad[data_voluntarios_3$edad > median(data_voluntarios_3$edad,na.rm = T) + 1.5*IQR(data_voluntarios_3$edad,na.rm = T) | data_voluntarios_3$edad < median(data_voluntarios_3$edad,na.rm = T) - 1.5*IQR(data_voluntarios_3$edad,na.rm = T)]
length(na.omit(outliersedad))

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

graphedad = ggplot(data_voluntarios_3, aes(x = edad)) + geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de las edades de los voluntarios registrados", x = "Edad", y = "Frecuencia") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
        scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50)) +
        scale_x_continuous(limits = c(0,80), breaks = seq(0,80, by = 10))

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


# Imputacion

# Lleno NA's con moda en columnas de Horas Diarias, Dias Semanales, Turno
modadias = as.character(names(sort(table(data_voluntarios_3$`Dias Semanales`), decreasing = TRUE)[1]))
modahoras = as.character(names(sort(table(data_voluntarios_3$`Horas Diarias`), decreasing = TRUE)[1]))
modaturno = as.character(names(sort(table(data_voluntarios_3$Turno), decreasing = TRUE)[1]))

data_voluntarios_3$Turno[is.na(data_voluntarios_3$Turno)] = modaturno
data_voluntarios_3$`Dias Semanales`[is.na(data_voluntarios_3$`Dias Semanales`)] = modadias
data_voluntarios_3$`Horas Diarias`[is.na(data_voluntarios_3$`Horas Diarias`)] = modahoras

table(data_voluntarios_3$Turno)






# Estudio de relaciones entre variables

# Numerica con categoricas

# Relacion entre edad y turno, si tiene las tardes libres es porque debe tener mas de 25
# Se compararan las medias de ambos grupos de personas, separados por su tipo de turno
t.test(data_voluntarios_3[data_voluntarios_3$Turno=="TARDE",]$edad,data_voluntarios_3[data_voluntarios_3$Turno=="MAÑANA",]$edad, alternative = 'greater')
# Hipotesis alternativa: La diferencia de las medias es mayor que cero
# No hay evidencia estadistica suficiente para afirmar dicha hipotesis
# alternativa, ya que su p-valor es 0.5685

# Relacion entre edad y origen, suponiendo que mientras el origen es mas "avanzado" mas grande es
t.test(data_voluntarios_3[data_voluntarios_3$Origen=="Recibidos",]$edad,data_voluntarios_3[data_voluntarios_3$Turno!="Recibidos",]$edad, alternative = 'less')
# Con un p-valor de 0.003 podemos afirmar que las personas que
# forman parte del grupo de recibidos, promedian una edad
# menor que las que no

t.test(data_voluntarios_3[data_voluntarios_3$Origen=="Historial",]$edad,data_voluntarios_3[data_voluntarios_3$Turno!="Historial",]$edad, alternative = 'greater')

t.test(data_voluntarios_3[data_voluntarios_3$Origen=="Actuales",]$edad,data_voluntarios_3[data_voluntarios_3$Turno!="Actuales",]$edad, alternative = 'less')
# Con un p-valor de 0.01 podemos afirmar que las personas que
# forman parte del grupo de actuales, promedian una edad
# menor que las que no

t.test(data_voluntarios_3[data_voluntarios_3$Origen=="Eventuales",]$edad,data_voluntarios_3[data_voluntarios_3$Turno!="Eventuales",]$edad, alternative = 'greater')
t.test(data_voluntarios_3[data_voluntarios_3$Origen=="Formulario",]$edad,data_voluntarios_3[data_voluntarios_3$Turno!="Formulario",]$edad, alternative = 'less')

# Relacion entre edad y dias semanales
cor.test(data_voluntarios_3$edad,as.numeric(data_voluntarios_3$`Dias Semanales`),method = "spearman")
# Este test plantea que la edad y la cantidad de dias semanales disponibles
# tienen cierta correlacion, ya que su hipotesis alternativa dice
# "la correlacion no es igual a cero", es decir, que existe
# en este caso no hay suficiente evidencia estadistica para afirmar dicha
# hipotesis, ya que su p-valor es 0.07108

# Relacion entre edad y horas diarias
cor.test(data_voluntarios_3$edad,as.numeric(data_voluntarios_3$`Horas Diarias`),method = "spearman")
# Este test plantea en su hipotesis alternativa que existe algun tipo de correlacion
# entre la edad y la cantidad de horas diarias que puede colaborar una persona
# aunque dicha hipotesis no tiene suficiente evidencia estadistica que la contraste

# Relacion entre dias semanales y horas diarias
chisq.test(table(data_voluntarios_3$`Dias Semanales`,data_voluntarios_3$`Horas Diarias`))
# El resultado de este test indica que existe una relacion significativa entre
# los dias semanales que tiene una persona y las horas diarias, ya que su p-valor
# es menor que 2.2e-16, es decir que hay evidencia estadistica suficinete para
# afirmar la hipotesis alternativa que plantea lo afirmado previamente.

# Relacion entre dias semanles y turno
chisq.test(table(data_voluntarios_3$`Dias Semanales`,data_voluntarios_3$Turno))
# El resultado de este test indica que existe una relacion significativa entre
# los dias semanales que tiene una persona y el turno del dia en el que se
# encuentra disponible , ya que su p-valor es menor que 2.2e-16,
# es decir que hay evidencia estadistica suficinete para
# afirmar la hipotesis alternativa que plantea lo afirmado previamente.

# Relacion entre horas diarias y turno
chisq.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$Turno))
# El resultado de este test indica que existe una relacion significativa entre
# las horas diarias que tiene una persona y el turno del dia en el que se
# encuentra disponible , ya que su p-valor es menor que 2.2e-16,
# es decir que hay evidencia estadistica suficinete para
# afirmar la hipotesis alternativa que plantea lo afirmado previamente.

table(data_voluntarios_3$Origen)


# Análisis exploratorio ---------------------------------------------------

# Análisis estadístico ---------------------------------------------------

# Relaciones entre variables ---------------------------------------------------

# Algoritmos ---------------------------------------------------
