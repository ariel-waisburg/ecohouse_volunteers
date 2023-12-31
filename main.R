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
data_voluntarios_2 <- data_voluntarios_2[order(data_voluntarios_2$Origen), ] %>% filter(Origen != "Historial")
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







# Análisis exploratorio ---------------------------------------------------

table(data_voluntarios_3$`Horas Diarias`) / length(data_voluntarios_3$`Dias Semanales`)
table(data_voluntarios_3$`Dias Semanales`)/ length(data_voluntarios_3$`Dias Semanales`)
table(data_voluntarios_3$Origen)/ length(data_voluntarios_3$`Dias Semanales`)
table(data_voluntarios_3$Turno)/ length(data_voluntarios_3$`Dias Semanales`)
table(data_voluntarios_3$`Tipo de Documento`)/ length(data_voluntarios_3$`Dias Semanales`)

bar_horas = ggplot(data_voluntarios_3, aes(x = `Horas Diarias`)) + geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Grafico de Barras de las Horas Diarias Disponibles", x = "Horas Diarias Disponibles", y = "Frecuencia") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  scale_y_continuous(limits = c(0, 1300), breaks = seq(0, 1300, by = 200))

bar_dias = ggplot(data_voluntarios_3, aes(x = `Dias Semanales`)) + geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Grafico de Barras de los Dias Semanales Disponibles", x = "Dias Semanales Disponibles", y = "Frecuencia") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200))

pie_turno = ggplot(data_voluntarios_3, aes(x = "",fill = Turno)) +
  geom_bar(width = 1, stat = "count", alpha = 0.8) +
  coord_polar(theta = "y") +
  labs(title = "Gráfico de Torta Frecuencia Turnos", fill = "Turno") +
  scale_fill_manual(values = c("blue","lightblue")) +
  scale_y_continuous(limits = c(0, 1428), breaks = seq(0, 1500, by = 250)) +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 10))

bar_estado = ggplot(data_voluntarios_3, aes(x = Origen)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Grafico de Barras de la Frecuencia de los Estados", x = "Estados", y = "Frecuencia") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 150))

data_voluntarios_3$nacionalidades_agrupadas = ifelse(!grepl("Argentina", data_voluntarios_3$Nacionalidad), "Extranjero", "Argentina")
pie_nacionalidad = ggplot(data_voluntarios_3, aes(x = "",fill = nacionalidades_agrupadas)) +
  geom_bar(width = 1, stat = "count", alpha = 0.8) +
  coord_polar(theta = "y") +
  labs(title = "Gráfico de Torta Frecuencia Nacionalidad", fill = "Nacionalidad") +
  scale_fill_manual(values = c("lightblue","blue")) +
  scale_y_continuous(limits = c(0, 1428), breaks = seq(0, 1500, by = 250)) +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 10))

pie_tipodocumento = ggplot(data_voluntarios_3, aes(x = "",fill = `Tipo de Documento`)) +
  geom_bar(width = 1, stat = "count", alpha = 0.8) +
  coord_polar(theta = "y") +
  labs(title = "Gráfico de Torta Frecuencia Tipo de Documento", fill = "Tipo de Documento") +
  scale_fill_manual(values = c("lightblue","blue")) +
  scale_y_continuous(limits = c(0, 1428), breaks = seq(0, 1500, by = 250)) +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 10))


# Análisis estadístico ---------------------------------------------------

summary(data_voluntarios_3$edad)

graphedad = ggplot(data_voluntarios_3, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de las edades de los voluntarios registrados", x = "Edad", y = "Frecuencia") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size = 30),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50)) +
  scale_x_continuous(limits = c(0,80), breaks = seq(0,80, by = 10))

# Relaciones entre variables ---------------------------------------------------

# ANALISIS DE ORIGEN/ESTADO:

## Comparacion medias de edades en relacion a las otras variables

### Rechaza
summary(aov(edad ~ Origen, data = data_voluntarios_3)) # cambia la media de las edades por Origen
TukeyHSD(aov(edad ~ Origen, data = data_voluntarios_3)) # la diferencia de las medias se da especificamente entre Eventuales y Recibidos}

ggplot(data_voluntarios_3, aes(x = edad, fill = Origen)) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Edad by Group",
       x = "Edad",
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(limits = c(15, 70), breaks = seq(15, 70, by = 5))

ggplot(subset(data_voluntarios_3, Origen %in% c("Recibidos", "Eventuales")), aes(x = edad, fill = Origen)) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Edad by Group",
       x = "Edad",
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(limits = c(15, 70), breaks = seq(15, 70, by = 5)) +
  geom_vline(xintercept = mean(data_voluntarios_3$edad[data_voluntarios_3$Origen == "Recibidos"]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(data_voluntarios_3$edad[data_voluntarios_3$Origen == "Eventuales"]), color = "lightblue4", linetype = "dashed")

### No rechaza: no modifica el promedio de edad por ninguna variable
summary(aov(edad ~ `Horas Diarias`, data = data_voluntarios_3))
summary(aov(edad ~ `Dias Semanales`, data = data_voluntarios_3))
summary(aov(edad ~ Turno, data = data_voluntarios_3))
summary(aov(edad ~ nacionalidades_agrupadas, data = data_voluntarios_3))
summary(aov(edad ~ `Tipo de Documento`, data = data_voluntarios_3))

## Disponibilidad horaria de las 3 con origen

### Rechaza: hay relacion entre estado y c/u de las variables.
fisher.test(table(data_voluntarios_3$nacionalidades_agrupadas,data_voluntarios_3$Origen))
mosaicplot(table(data_voluntarios_3$nacionalidades_agrupadas,data_voluntarios_3$Origen), color = TRUE)
fisher.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$Origen))
mosaicplot(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$Origen), color = TRUE)
fisher.test(table(data_voluntarios_3$Turno, data_voluntarios_3$Origen))
mosaicplot(table(data_voluntarios_3$Turno,data_voluntarios_3$Origen), color = TRUE)
fisher.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$Origen))
mosaicplot(table(data_voluntarios_3$Turno,data_voluntarios_3$`Horas Diarias`), color = TRUE)

## Arboles
data_arbol <- data_voluntarios_3 %>% select(-c(2, 3, 4, 5, 6))
data_arbol_2 <- data_voluntarios_2_new %>% mutate(Estado = ifelse((Origen == "Recibidos" | Origen == "Actuales"), "Activo", "Inactivo")) %>% select(-1)

data_arbol %>% explore_all(target = Origen)

arbol_1 <- rpart(Origen~., data = data_arbol, method = "class")
rpart.plot(arbol_1, main = "Arbol de Clasificacion: origen")

arbol_2 <- rpart(Estado~., data = data_voluntarios_2_new, method = "class")
rpart.plot(arbol_2, main = "Arbol de Clasificacion: estado")

## Conclusiones

1. Hay diferencia de medias de edades entre estados, especificamente entre Eventuales y Recibidos.
2. Hay diferencia de medias de edades entre estados, especificamente entre Eventuales y Recibidos.
3. Hay relacion entre estado y: nacionalidad arg/ext, horas diarias y turno.
4.



# Clustering
data_voluntarios_num <- data.frame(apply(data_voluntarios_3 %>% select(7, 8, 10), 2, function(i) as.numeric(i)))
data_voluntarios_num_scaled <- scale(data_voluntarios_num)

## Analizar la utilidad del clustering: estudio de tendencia.
H = 1 - hopkins(data_voluntarios_num_scaled)

dist_data_voluntarios <- dist(data_voluntarios_num_scaled, method = "euclidean")

fviz_dist(dist.obj = dist_data_voluntarios, show_labels = FALSE) +
  labs(title = "Heatmap de distancias euclídeas ordenadas") + theme(legend.position = "bottom")

n_clusters <- NbClust(data = data_voluntarios_num_scaled, distance = "euclidean", min.nc = 2,
                      max.nc = 10, method = "kmeans", index = "all")

clusters <- kmeans(x = dist_data_voluntarios, centers = 4, nstart = 50)
data_voluntarios_cluster <- data_voluntarios_3 %>% mutate(cluster = as.factor(clusters$cluster))

pca <- prcomp(data_voluntarios_num_scaled)
summary(pca)
screeplot(pca)
fviz_pca_biplot(
  pca,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 2,
  fill.ind = data_students_cluster$cluster,
  addEllipses = TRUE,
  legend.title = "Cluster"
)


fviz_cluster <- fviz_cluster(object = clusters, data = data_voluntarios_num_scaled, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

# FIJARSE FISHER RECHAZA SIEMPRE

# Entre las 3 de disponibilidad horaria

# Rechaza
chisq.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$`Dias Semanales`))
fisher.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$`Dias Semanales`))

# Rechaza
chisq.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$Turno))
fisher.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$Turno))

# Rechaza
chisq.test(table(data_voluntarios_3$Turno,data_voluntarios_3$`Dias Semanales`))
fisher.test(table(data_voluntarios_3$Turno,data_voluntarios_3$`Dias Semanales`))

# Las 3 con nacionalidad
chisq.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$nacionalidades_agrupadas))
fisher.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$nacionalidades_agrupadas))

chisq.test(table(data_voluntarios_3$Turno,data_voluntarios_3$nacionalidades_agrupadas))
fisher.test(table(data_voluntarios_3$Turno, data_voluntarios_3$nacionalidades_agrupadas))

chisq.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$nacionalidades_agrupadas))
fisher.test(table(data_voluntarios_3$`Horas Diarias`,data_voluntarios_3$nacionalidades_agrupadas))

# Nacionalidad con origen
chisq.test(table(data_voluntarios_3$Origen,data_voluntarios_3$nacionalidades_agrupadas))
# Rechaza revisar
fisher.test(table(data_voluntarios_3$Origen,data_voluntarios_3$nacionalidades_agrupadas))



# Algoritmos ---------------------------------------------------


# PCA

data_voluntarios_num <- apply(data_voluntarios_3 %>% select(7, 8), 2, function(i) as.numeric(i))
data_voluntarios_num <- cbind(data_voluntarios_num, as.numeric(data_voluntarios_3$Origen), as.numeric(data_voluntarios_3$Turno))
colnames(data_voluntarios_num)[3:4] = c("Origen", "Turno")
cov(data_voluntarios_num, method = "pearson")
cov(data_voluntarios_num, method = "spearman")

corrplot::corrplot.mixed(cor(data_voluntarios_num, method = "pearson"))
corrplot::corrplot.mixed(cor(data_voluntarios_num, method = "spearman"))

data_voluntarios_num_scaled <- scale(data_voluntarios_num)

pca <- prcomp(data_voluntarios_num_scaled)
summary(pca)
screeplot(pca)
fviz_pca_biplot(
  pca,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 2,
  fill.ind = data_students_cluster$cluster,
  addEllipses = TRUE,
  legend.title = "Cluster"
)


# MCA
data_voluntarios_cat <- data_voluntarios_3 %>% select(4,7,8,9)
data_voluntarios_cat$Nacionalidad <- factor(ifelse(data_voluntarios_3$Nacionalidad == "Argentina", "Argentina", "OTRO"))
mca <- MCA(data_voluntarios_cat, graph = TRUE)
fviz_screeplot(mca, addlabels = TRUE, ncp = 12, ylim = c(0, 50))



data <- data_voluntarios %>%
  select(1, 3, 6, 7, 8, 9) %>%
  mutate(Estado = ifelse((Origen == "Recibidos" | Origen == "Actuales"), "Activo", "Inactivo"), edad = year(Sys.Date()) - year(`Fecha de Nacimiento`)) %>%
  select(-c(1,2))

data %>% describe()

colSums(is.na(data))

sum(rowSums(is.na(data[, c(1)])) == length(c(1)))


data[which(rowSums(is.na(data[, c(2,3)])) == length(c(2,3))),] %>% group_by(Nacionalidad) %>% summarise(n = n())
data[which(rowSums(is.na(data[, c(2,3)])) == length(c(2,3))),] %>% group_by(Estado) %>% summarise(n = n())
mean((data[-which(rowSums(is.na(data[, c(2,3)])) == length(c(2,3))),]$edad)[!is.na(data[-which(rowSums(is.na(data[, c(2,3)])) == length(c(2,3))),]$edad)])

which(rowSums(is.na(data[, c(2,3,4)])) == length(c(2,3,4)))

which(rowSums(is.na(data[, c(2,3,4,6)])) == length(c(2,3,4,6)))

data_comp <- data[complete.cases(data),] %>% mutate_if(is.character, as.factor)

### Rechaza: hay relacion entre estado y c/u de las variables.
#fisher.test(table(data_voluntarios_3$nacionalidades_agrupadas,data_voluntarios_3$Origen))
#mosaicplot(table(data_voluntarios_3$nacionalidades_agrupadas,data_voluntarios_3$Origen), color = TRUE)
fisher.test(table(data_comp$`Horas Diarias`,data_comp$Estado))
mosaicplot(table(data_comp$`Horas Diarias`,data_comp$Estado), color = TRUE)
fisher.test(table(data_comp$Turno,data_comp$Estado))
mosaicplot(table(data_comp$Turno,data_comp$Estado), color = TRUE)


arbol_3 <- rpart(Estado~., data = data_comp, method = "class")
rpart.plot(arbol_3, main = "Arbol de Clasificacion: estado")

# Count the number of rows meeting the condition

# Tests viejos ------------------------------------------------------------

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



t.test(data_voluntarios_3$edad[data_voluntarios_3$Origen=="Recibidos"],data_voluntarios_3$edad[data_voluntarios_3$Origen != "Recibidos"],alternative = "less")


table(data_voluntarios_3$Origen)
# Relacion entre edad y dias semanales
cor.test(data_voluntarios_3$edad,as.numeric(data_voluntarios_3$Origen),method = "pearson")
plot(data_voluntarios_3$edad,as.numeric(data_voluntarios_3$Origen))
