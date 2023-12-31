# Import packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(mice)
library(naniar)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(hopkins)
library(NbClust)
library(cluster)
library(rpart)
library(rpart.plot)
library(explore)
library(mice)

# Import data ----------------------------------------------------------------
data_voluntarios <- read_excel("Nueva Base (3).xlsx", sheet = "Principal", col_types = c("text",
                                                                           "date", "date", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "text", "text", "text", "text", "text",
                                                                           "numeric")) %>% select(1:11) %>% select(-6)

# Limpieza ----------------------------------------------------------------

data_voluntarios <- data_voluntarios %>%
  mutate(`Dias Semanales` = ifelse(grepl("^Días de semana por la tarde", `Disponibilidad Horaria`), 4, `Dias Semanales`),
         `Horas Diarias` = ifelse(grepl("^Días de semana por la tarde", `Disponibilidad Horaria`), 3, `Horas Diarias`),
         Turno = ifelse(grepl("^Días de semana por la tarde", `Disponibilidad Horaria`), "TARDE", Turno))

data_voluntarios <- data_voluntarios %>%
  mutate(`Dias Semanales` = ifelse(grepl("^Fines de semana y feriados", `Disponibilidad Horaria`), 2, `Dias Semanales`),
         `Horas Diarias` = ifelse(grepl("^Fines de semana y feriados", `Disponibilidad Horaria`), 3, `Horas Diarias`),
         Turno = ifelse(grepl("^Fines de semana y feriados", `Disponibilidad Horaria`), NA, Turno))

# Tipos de datos y validaciones
data_voluntarios_2 <- data_voluntarios[,-ncol(data_voluntarios)] %>% mutate_if(is.character, as.factor)
data_voluntarios_2$Nacionalidad <- ifelse(data_voluntarios$Nacionalidad == "#N/A", NA, data_voluntarios$Nacionalidad)
data_voluntarios_2$`Horas Diarias` <- factor(as.integer(ifelse(grepl("[A-Za-z]", data_voluntarios$`Horas Diarias`), NA, data_voluntarios$`Horas Diarias`)))
data_voluntarios_2$`Dias Semanales` <- factor(as.integer(ifelse(grepl("[A-Za-z]", data_voluntarios$`Dias Semanales`), NA, data_voluntarios$`Dias Semanales`)))
data_voluntarios_2$`Número de Documento NEW` <- toupper(str_replace_all(data_voluntarios_2$`Número de Documento NEW`, "\\s", ""))
fecha_edad_minima <- Sys.Date() - years(15)
data_voluntarios_2 <- data_voluntarios_2[data_voluntarios_2$`Fecha de Nacimiento` < fecha_edad_minima,]
orden_permanencia = c("Recibidos","Actuales","Eventuales","Formulario", "Historial")
data_voluntarios_2$Origen <- factor(data_voluntarios_2$Origen, levels = orden_permanencia)
data_voluntarios_2 <- data_voluntarios_2[order(data_voluntarios_2$Origen), ]
data_voluntarios_2 <- data_voluntarios_2[!duplicated(data_voluntarios_2[,-c(1,2,9)]),]
data_voluntarios_2 <- data_voluntarios_2[!is.na(data_voluntarios_2$`Número de Documento NEW`), ]
extra <- data_voluntarios_2 %>%
  group_by(`Número de Documento NEW`) %>%
  filter(n() > 1) %>%
  arrange(`Número de Documento NEW`)
extra <- extra[order(extra$Origen),]
extra <- extra[duplicated(extra[,-c(1,2,7,8,9)]),]
data_voluntarios_2 <- data_voluntarios_2 %>%
  anti_join(extra, by = c("Origen", "Tipo de Documento", "Número de Documento NEW", "Nacionalidad", "Horas Diarias", "Dias Semanales", "Turno"))
data_voluntarios_2 <- data_voluntarios_2[-which(rowSums(is.na(data_voluntarios_2[, c(6,7,8,9)])) == length(c(6,7,8,9))),]
data_voluntarios_2
data_voluntarios_2_new <- data_voluntarios_2 %>%
  mutate(
    Estado = factor(ifelse((Origen == "Recibidos" | Origen == "Actuales"), "Activo", "Inactivo")),
    Edad = year(Sys.Date()) - year(`Fecha de Nacimiento`),
    Argentino = factor(ifelse(!grepl("Argentina", Nacionalidad), 0, 1))
  ) %>%
  select(7,8,9,10,11,12) %>%
  mutate_if(is.character, as.factor)
data_voluntarios_2_new
View(data_voluntarios_2_new)

solo_nacio <- which(rowSums(is.na(data_voluntarios_2[, c(7,8,9)])) == length(c(7,8,9)))
data_voluntarios_3 <- data_voluntarios_2[-solo_nacio,]
data_voluntarios_3
data_voluntarios_3_new <- data_voluntarios_2_new[-solo_nacio,]
data_voluntarios_3_new
View(data_voluntarios_3_new)

sin_turno <- which(rowSums(is.na(data_voluntarios_3[, c(9)])) == length(c(9)))
data_voluntarios_4 <- data_voluntarios_3[-sin_turno,]
data_voluntarios_4
data_voluntarios_4_new <- data_voluntarios_3_new[-sin_turno,]
data_voluntarios_4_new
View(data_voluntarios_4_new)

# para ver las freq. relativas
table(data_voluntarios_3$Origen) / length(data_voluntarios_3$Origen) * 100

# Outliers ----------------------------------------------------------------

#data_voluntarios_3 = data_voluntarios_3 %>% mutate(edad = year(Sys.Date()) - year(`Fecha de Nacimiento`))
#outliersedad = data_voluntarios_3$edad[data_voluntarios_3$edad > median(data_voluntarios_3$edad,na.rm = T) + 1.5*IQR(data_voluntarios_3$edad,na.rm = T) | data_voluntarios_3$edad < median(data_voluntarios_3$edad,na.rm = T) - 1.5*IQR(data_voluntarios_3$edad,na.rm = T)]
#length(na.omit(outliersedad))

# Missings ----------------------------------------------------------------
evaluar_missings <- function(data) {
  md.pattern(data_voluntarios_3_new, rotate.names = TRUE)

  cor(data_voluntarios_3_new %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric), use = "pairwise.complete.obs", method = c("pearson")) %>% round(digits = 2)
  cor(data_voluntarios_3_new %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric), use = "complete.obs", method = c("pearson")) %>% round(digits = 2)
  cor(data_voluntarios_3_new %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric), use = "complete.obs", method = c("spearman")) %>% round(digits = 2)

  dummyNA <-as.data.frame(abs(is.na(data_voluntarios_3_new)))

  cor(dummyNA) %>% round(digits = 2)

  cor(data_voluntarios_3_new %>% mutate_if(~ is.factor(.) || is.character(.) || is.POSIXct(.), as.numeric),
      dummyNA, use = "pairwise.complete.obs") %>%
    round(digits = 2)
}

evaluar_missings(data_voluntarios_2_new)
evaluar_missings(data_voluntarios_3_new)

# Tests para ver si hay diferencias significativas entre los valores ausentes y no ausentes.

# Del 2

# De Nacionalidad
mcar_test(data_voluntarios_2_new %>% select(Argentino, `Horas Diarias`)) # No sign.
mcar_test(data_voluntarios_2_new %>% select(Argentino, `Dias Semanales`))
mcar_test(data_voluntarios_2_new %>% select(Argentino, Turno)) # No sign.
mcar_test(data_voluntarios_2_new %>% select(Argentino, Estado))
mcar_test(data_voluntarios_2_new %>% select(Argentino, Edad))

# De Edad
mcar_test(data_voluntarios_2_new %>% select(Edad, `Horas Diarias`)) # No sign.
mcar_test(data_voluntarios_2_new %>% select(Edad, `Dias Semanales`)) # No sign.
mcar_test(data_voluntarios_2_new %>% select(Edad, Turno)) # No sign.
mcar_test(data_voluntarios_2_new %>% select(Edad, Estado))

# De Estado
mcar_test(data_voluntarios_2_new %>% select(Estado, `Horas Diarias`))
mcar_test(data_voluntarios_2_new %>% select(Estado, `Dias Semanales`))
mcar_test(data_voluntarios_2_new %>% select(Estado, Turno))

# De Turno
mcar_test(data_voluntarios_2_new %>% select(Turno, `Horas Diarias`))
mcar_test(data_voluntarios_2_new %>% select(Turno, `Dias Semanales`))

# Dias Semanales
mcar_test(data_voluntarios_2_new %>% select(`Dias Semanales`, `Horas Diarias`))


# Del 3

# De Nacionalidad
mcar_test(data_voluntarios_3_new %>% select(Argentino, `Horas Diarias`)) # No sign.
mcar_test(data_voluntarios_3_new %>% select(Argentino, `Dias Semanales`)) # No sign.
mcar_test(data_voluntarios_3_new %>% select(Argentino, Turno)) # No sign.
mcar_test(data_voluntarios_3_new %>% select(Argentino, Estado))
mcar_test(data_voluntarios_3_new %>% select(Argentino, Edad))

# De Edad
mcar_test(data_voluntarios_3_new %>% select(Edad, `Horas Diarias`))
mcar_test(data_voluntarios_3_new %>% select(Edad, `Dias Semanales`)) # No sign.
mcar_test(data_voluntarios_3_new %>% select(Edad, Turno)) # No sign.
mcar_test(data_voluntarios_3_new %>% select(Edad, Estado))

# De Estado
mcar_test(data_voluntarios_3_new %>% select(Estado, `Horas Diarias`))
mcar_test(data_voluntarios_3_new %>% select(Estado, `Dias Semanales`))
mcar_test(data_voluntarios_3_new %>% select(Estado, Turno))

# De Turno
mcar_test(data_voluntarios_3_new %>% select(Turno, `Horas Diarias`))
mcar_test(data_voluntarios_3_new %>% select(Turno, `Dias Semanales`))

# Dias Semanales
mcar_test(data_voluntarios_3_new %>% select(`Dias Semanales`, `Horas Diarias`))


model_independiente = glm(
  is.na(data_voluntarios_3_new$Turno) ~.,
  data = data_voluntarios_3_new %>% select(Estado, `Horas Diarias`, `Dias Semanales`),
  family = 'binomial'
)
model_independiente %>% summary()

data_regresion_2 <- data_voluntarios_2_new %>% mutate_if(is.factor, as.numeric)
colnames(data_regresion_2) <- c("Horas_Diarias","Dias_Semanales","Turno","Estado","Edad","Argentino")
regImp <- mice(subset(data_regresion_2, select = c(Horas_Diarias, Dias_Semanales, Turno, Estado)), m = 1, printFlag = F, seed = 123)
coro_regImp <- complete(regImp, 1)
summary(coro_regImp)

data_regresion_3 <- data_voluntarios_3_new %>% mutate_if(is.factor, as.numeric)
colnames(data_regresion_3) <- c("Horas_Diarias","Dias_Semanales","Turno","Estado","Edad","Argentino")
regImp_2 <- mice(subset(data_regresion_3, select = c(Horas_Diarias, Dias_Semanales, Turno, Estado)), m = 1, printFlag = F, seed = 123)
coro_regImp_2 <- complete(regImp_2, 1)
summary(coro_regImp_2)
