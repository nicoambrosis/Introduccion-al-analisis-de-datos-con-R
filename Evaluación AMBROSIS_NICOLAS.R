#############################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biología Computacional
#                         Diciembre 2021
#                      Evaluación del curso
#
#Para organizarnos mejor:
#Entregar un script de R cuyo nombre sea apellido_nombre.R con la resolución
#de los ejercicios a andresrabinovich@gmail.com y con el subject "Evaluación R".
#Pueden realizarnos consultas a ese mismo email con el subject "Consultas Evaluación R".
#############################################################################

##################################################################################################
#ABSTRACT
#Los animales durante su desarrollo se enfrentan con problemas en la asignación de recursos cuando reciben cantidades subóptimas
#de los mismos en ambientes naturales muy variables.
#Estos problemas se manifiestan posiblemente en el intercambio (trade-off) entre crecimiento y mantenimiento somático.
#Sin embargo, la medida en que se manifiesta este intercambio sigue siendo una pregunta abierta.
#Para intentar responder a esta pregunta, se inyectó a pichones de papamoscas cerrojillo (Ficedula hypoleuca) con
#IGF-1 (insulin-like growth factor 1) para observar como IGF-1 afecta el despliegue de una respuesta antioxidante mediada por
#la enzima glutathione peroxidasa (GPx).
#Se midió la actividad de GPx y el peso en pichones tratados y en pichones no tratados (control)

#################Consignas########################
#Cada ejercicio otorga 1 punto.

#Problema 1: Abrir el dataset nestlings.csv en RStudio y describirlo exhaustivamente para comprender el contenido del mismo.
#ej: cantidad de observaciones, cantidad de atributos, tipo de atributos, etc.

df_pichones <- read.csv("nestlings.csv")
class(df_pichones) 

head(df_pichones)
tail(df_pichones)
dim(df_pichones) # 66 observaciones (filas) y 5 variables o atributos (columnas)
sapply(df_pichones, class) # esta es una manera sencilla de analizar que tipo de datos hay 
                            # en cada columna.



colnames(df_pichones)
unique(df_pichones$manipulation)
length(unique(df_pichones$nestling_ID))
unique(df_pichones$age)


# dos formas de chequear si hay NAs en el dataset.
any(is.na(df_pichones)) 
complete.cases(df_pichones)


any(duplicated(df_pichones)) # no hay filas duplicadas
summary(df_pichones)



#Problema 2: Calcular cuántas observaciones hay de Control y cuántas de IGF.¿Los datos están
#balanceados (cantidades similares de observaciones de ambos tipos)?
table(df_pichones$manipulation)
# al ejecutar esta linea vemos que el grupo control y el grupo tratado tiene el mismo numero
# de observaciones. Por lo tanto los datos estan bien balanceados.




#Problema 3: Calcular media, mediana, desvío estándar y distancia inter cuartil para GPx_activity
#y sample_weight. Realizar esta descripción para cada tratamiento por separado.
#¿Las medidas de centralidad son similares para cada tratamiento y para cada atributo? ¿Y las de
#dispersión?
unique(df_pichones$manipulation)

# Con estas lineas definimos dos objetos que van a funcionar como filtros de la DF y nos van a permitir 
# simplificar las lineas de codigo.
Control <- df_pichones$manipulation=="Control-injected"
IGF_1 <- df_pichones$manipulation=="IGF-1-injected"

####### GPx ###############
# Mean
mean(df_pichones$GPx_activity[Control])
mean(df_pichones$GPx_activity[IGF_1])

# Median
median(df_pichones$GPx_activity[Control])
median(df_pichones$GPx_activity[IGF_1])

# IQR
IQR(df_pichones$GPx_activity[Control])
IQR(df_pichones$GPx_activity[IGF_1])


####### SAMPLE WEIGHT ###############
# Mean
mean(df_pichones$sample_weight[Control])
mean(df_pichones$sample_weight[IGF_1])

# Median
median(df_pichones$sample_weight[Control])
median(df_pichones$sample_weight[IGF_1])

# IQR
IQR(df_pichones$sample_weight[Control])
IQR(df_pichones$sample_weight[IGF_1])


# En primer lugar vemos que los datos correspondientes al grupo tratado con IGF_1 presentan una dispersion
# sensiblemente mayor que los datos del grupo control. Antes de aplicar algun test estadistico,
# y  considerando los valores de IQR, podemos suponer que la diferencia entre medias de ambos grupos no 
# no seria estadisticamente significativa. 






#Problema 4: Realizar un boxplot y un histograma de GPx_activity para cada tratamiento. 
#Retirar los datos que son muy extremos (decidan ustedes qué consideran muy extremos)
#(Ayuda: no hay que retirar solamente el valor de GPx_activity, hay que retirar toda la fila del valor atípico)
#Por ejemplo, supongamos que la observación 100 tiene un GPx_activity atípico, entonces:
#nestlingsSinAtipicos <- nestlings[-100, ] #Es decir, sacamos toda la fila.
#A partir de acá, seguir trabajando con el nuevo dataframe con los datos atípicos removidos.


layout(matrix(1:2, ncol = 2))
############## BOXPLOT #################

boxplot(df_pichones$GPx_activity ~ df_pichones$manipulation,
        main = "BoxPlot",
        xlab  = "",
        ylab = "GPx Activity",
        col=c("#9c0f05","#003d75"),
        whisklty = 2, # tipo de bigote
        outpch = 19, #simbolo para los outliars
        )



stripchart(df_pichones$GPx_activity ~ df_pichones$manipulation,
           method = "jitter",
           vertical = T,
           pch = 19,
           add = F, #si queremos superponerlo sobre el BoxPlot >> add = True
           col=c("#9c0f05","#003d75"),
           main = "StripChart")       

layout(1)

# en ambos grupos hay datos que estan por fuera de los bigotes del boxplot.
# Los datos anomalos corresponden a las observaciones 35, 49 y 61. Las eliminamos del DataSet y guardamos 
# el nuevp DataSet como un nuevo archivo.


max(df_pichones$GPx_activity[Control])
max(df_pichones$GPx_activity[IGF_1])
min(df_pichones$GPx_activity[Control])


# eliminamos las filas (35, 49 y 61) que contienen los datos anomalos.
df_pichonesOK <- df_pichones[-c(35, 49, 61),]

# con esta linea chequeamos que eliminamos 3 datos y las dimensiones de la tabla cambiaron 
dim(df_pichonesOK)

# con estas lineas chequeamos que eliminamos los datos que queriamos eliminar.
max(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="Control-injected"])
min(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="Control-injected"])
max(df_pichonesOK$GPx_activity[IGF_1 <- df_pichonesOK$manipulation=="IGF-1-injected"])


# confirmamos graficamente que los datos fueron eliminados
boxplot(df_pichonesOK$GPx_activity ~ df_pichonesOK$manipulation,
        main = "BoxPlot",
        xlab  = "",
        ylab = "GPx Activity",
        col=c("#9c0f05","#003d75"),
        whisklty = 2, # tipo de bigote
        outpch = 19, #simbolo para los outliars
) 

stripchart(df_pichonesOK$GPx_activity ~ df_pichonesOK$manipulation,
           method = "jitter",
           vertical = T,
           pch = 19,
           add = T, #si queremos superponerlo sobre el BoxPlot >> add = True
           col=c("#eb8f88","#85b3de"),
           )       



#################### HISTOGRAMA ############################


layout(matrix(1:2, ncol = 2))
# histograma izquierda
min(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="Control-injected"])
max(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="Control-injected"])

hist(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="Control-injected"],
     main = "Control-injected",
     xlab = "GPx_ activity",
     col = "#820303",
     xlim = c(1,5),
     )

#histograma derecha
min(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="IGF-1-injected"])
max(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="IGF-1-injected"])
hist(df_pichonesOK$GPx_activity[df_pichonesOK$manipulation=="IGF-1-injected"],
     main = "IGF-1-injected",
     xlab = "GPx_ activity",
     col = "#038082",
     xlim = c(1,5))

layout(1) # esta linea restituye el area de graficos a su forma normal de UN solo grafico 

#Problema 5: Visualizar en un mismo gráfico de boxplot el sample_weight para cada tratamiento. 
#Retirar los datos que son muy extremos (decidan ustedes qué consideran muy extremos)
#Ayuda, para decirle a un boxplot que grafique una variable 'y' pero separando por la variable 'x', pueden usar
#formulas: boxplot(y ~ x).
#A partir de acá, seguir trabajando con el nuevo dataframe con los datos atípicos removidos.

boxplot(df_pichonesOK$sample_weight ~ df_pichonesOK$manipulation,
        main = "BoxPlot",
        xlab  = "",
        ylab = "Sample Weight",
        col=c("#005411","#9c028a"),
        whisklty = 2, # tipo de bigote
        outpch = 19, #simbolo para los outliars
) 

stripchart(df_pichonesOK$sample_weight ~ df_pichonesOK$manipulation,
           method = "jitter",
           vertical = T,
           pch = 19,
           add = T, #si queremos superponerlo sobre el BoxPlot >> add = True
           col=c("#99d1a4","#e3a1db"),
)       

# vemos que hay un dato anomalo en el grupo Control. Lo vamos a eliminar

min(df_pichonesOK$sample_weight)

#eliminamos el dato.
df_pichonesOK <- df_pichonesOK[-27,]

dim(df_pichonesOK)
min(min(df_pichonesOK$sample_weight))

# analizamos graficamente.
boxplot(df_pichonesOK$sample_weight ~ df_pichonesOK$manipulation,
        main = "BoxPlot",
        xlab  = "",
        ylab = "Sample Weight",
        col=c("#005411","#9c028a"),
        whisklty = 2, # tipo de bigote
        outpch = 19, #simbolo para los outliars
) 

stripchart(df_pichonesOK$sample_weight ~ df_pichonesOK$manipulation,
           method = "jitter",
           vertical = T,
           pch = 19,
           add = T, #si queremos superponerlo sobre el BoxPlot >> add = True
           col=c("#99d1a4","#e3a1db"),
)       


write.csv(df_pichonesOK, file = "nestlings_OK.csv")



#Problema 6: Para poder determinar si existe una diferencia significativa en el peso entre estos los grupos, control y tratamiento, realizar un T test.
#¿A qué conclusión se llega?
#Realizar previamente todos los tests necesarios para garantizar que es válido aplicar un t test.
#Ayuda: para el test de homogeneidad de varianza, bartlett.test, es necesario previamente
#agrupar las observaciones en una lista. Si control es una variable con los pesos de los polluelos
#de control y tratamiento es una variable con los pesos de los polluelos tratados, generar una lista de la siguiente
#manera: total <- list(control, tratamiento) y utilizar esa lista en el test.
df_pichonesOK <- read.csv("nestlings_OK.csv")


Weight_control <- df_pichonesOK$sample_weight[df_pichonesOK$manipulation == "Control-injected"]
Weight_tratamiento <- df_pichonesOK$sample_weight[df_pichonesOK$manipulation == "IGF-1-injected"]

# Test de shapiro: Para testear que cada variable sea normal
#H0: Los datos tienen distribucion normal
#H1: Los datos no tienen distribucion normal
shapiro.test(Weight_control)
shapiro.test(Weight_tratamiento)



# Test de bartlett: Homogeneidad de varianza
#H0: Los datos tienen igual varianza
#H1: Los datos no tienen igual varianza
bartlett.test(list(Weight_control, Weight_tratamiento))
var(Weight_control)
var(Weight_tratamiento)
# el resultado(p-value = 0.03057) indica que las varianzas de los dos grupos experimentales
# son significativamente diferentes. Por lo tanto no podemos aplicar el t-test para 
# comparar estos datos.

# Dado el resultado que obtuvimos al analizar los supuestos, vamos a realizar el test de welch
# Es equivalente a t-test pero para comparr dos muestras que tienen varianzas diferentes.

t.test(Weight_control, Weight_tratamiento, var.equal = F)
# el resultado del t-test (p-value = 0.04405) indica que la diferencia entre las medias de ambos grupos
# experimentales son significativamente diferentes. Analizando los resultados graficamente y considerando
# el intervalo de confianza (95 percent confidence interval: 0.03810449 2.72120743) vemos que si bien la
# diferencia es estadisticamente significativa, tal vez no tenga una significacncia biologica muy imortante.





#Problema 7: Realizar un scatter plot entre GPx activity y sample weight ¿Puede observar algún tipo de relación entre estas dos variables?


plot(x = df_pichonesOK$GPx_activity,
     y = df_pichonesOK$sample_weight,
     xlab = "GPx Activity",
     ylab = "Sample weight",
     main = "Scatter Plot",
     pch = 19,
     col = "#4690eb",
     )
# a simple vista no se observa una realicion entre las dos variables.



#Problema 8: Realizar un ajuste lineal entre GPx activity y sample weight y graficarlo.
#Ayuda: funcion lm

# 1) Definimos la formula

formula <- df_pichonesOK$GPx_activity ~ df_pichonesOK$sample_weight

# 2) Hacemos el ajute lineal
ajuste <- lm(formula = formula, data = df_pichonesOK)

# 3) Analizamos los resultados
summary(ajuste)
# notar que el R-squared da muy mal (0.1107) lo que suiere que el ajuste lineal no seria una
# buena opcion
abline(ajuste, col="red")

# No entiendo por que el ajuste lineal de estos datos me da una recta que va muy por debajo
# de los datos (se ve bien si se ampl�a los "xlim" e  "ylim" del gr�fico).


#Problema 9: Cargar el dataset vinos.RData.
#El mismo consiste en los resultados del analisis quimico de vinos cultivados en la misma region de Italia pero en tres cultivares diferentes.
#Realizar un PCA y graficarlo. ¿Cuánta variabilidad explican las primeras 2 componentes? ¿Será una buena representación del dataset un gráfico 2d con estas primeras dos componentes?

View(vinos)
dim(vinos)
colnames(vinos)
any(is.na(vinos)) # no hay NAs
complete.cases(vinos)
any(duplicated(vinos)) # no hay filas duplicadas


vinos.pca <- prcomp(vinos, center = T, scale. = T)
vinos.pca

plot(vinos.pca$x[,1:2],
     main = "Vinos",
     pch = 19,
     col = "#820000")

abline(h = 0)
abline(v = 0)
summary(vinos.pca)
# La variacion total de los datos consta de 12 dimensiones (todas las variables medidas)
# Aplicamos PCA con el objetivo de reducir la dimensionalidad de estos datos
# Los resultados muestran que el compoente 1 (PC1) explica un 36,2% de la variabilidad de los datos,
# el componente 2 (PC2) explica un 19,29% y PC3 un 12,98%. En total los primeros 2 componentes
# explican un 55,59% de la variabilidad mientras que considerando los primeros 3 componentes
# llegamos a un 68,46% de variabilidad (datos obtenidos analizando summry(vinos.pca)). 
# Vemos que aun quda un porcentaje importante de la variabilidad explicado por las otras dimensiones
# En concluision considero que una representacion grafica del DataSet considerando solo las 2 primeras
# componentes no seria una buena representacion grafica ya que estariamos dejando fuera muchisima
# informacion.

plot(vinos.pca$sdev/sum(vinos.pca$sdev)*100,
     main = "Grafico del codo",
     xlab = "# de variable",
     ylab = "Porcentaje de variabilidad explicada",
     type = "b")


#Problema 10: #Cual es el vino más cercano al 121? Y al 137?
#Clusterizar con kmeans. Elegir un k con el criterio del codo.
#Realizar un silohuette y decidir si todos los datos quedaron agrupados en grupos compactos.
#Graficar el PCA coloreando por clusters.


# con estas dos lineas extraemos los numeros de las filas que representan a cada observacion
nombres <- rownames(vinos)
nombres

plot(vinos.pca$x[,1:2],
     main = "Vinos",
     pch = 19,
     col = "#820000",
     )
abline(h = 0)
abline(v = 0)



text(vinos.pca$x[, 1:2],
     cex=0.65,
     pos=2,
     col="black",
     nombres)



vinos_escalado <- as.data.frame(scale(vinos, center = T, scale = T))
distancia <- dist(vinos_escalado, method = "euclidean")
distancia

dendrograma <- hclust(distancia, 
                      method = "complete")

plot(dendrograma,
     hang = -1)
# Si analizamos el dendrograma vemos que la muestra mas cercana a la 121 es la 25 y a la 137 es
# la 127.

# pero si naizamos el PCA (PC1 Vs PC2) vemos que la muetra mas cercana a la 121 parece ser la 16
# y la mas cercana a la 137 parece ser la 155.


set.seed(1234567)
sse <- c()
clusters <- kmeans(vinos.pca$x[,1:2], centers = 2)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(vinos.pca$x[,1:2], centers = 3)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(vinos.pca$x[,1:2], centers = 4)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(vinos.pca$x[,1:2], centers = 5)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(vinos.pca$x[,1:2], centers = 6)
sse      <- c(sse, sum(clusters$withinss))
plot(2:6, sse, type = "b", xlab = "k") #Criterio del codo
# Segun el grafico del codo necesitariamos usar k <- 3

library(cluster)
K <- 3

set.seed(1234567)
clusters <- kmeans(vinos_escalado, centers = K)
clusters

#Grafiquemos a que grupo quedo asignado cada punto
plot(vinos.pca$x[,1:2],
     col=c("red", "blue", "green")[clusters$cluster],
     pch = 19)

#Grafico un silhouette
plot(silhouette(clusters$cluster, dist(vinos.pca$x[,1:2])))
# dado el resultado del grafico de siluetas vamos a repetir el analisis pero para k <- 2

K <- 2

set.seed(1234567)
clusters <- kmeans(vinos_escalado, centers = K)
clusters

#Grafiquemos a que grupo quedo asignado cada punto
plot(vinos.pca$x[,1:2],
     col=c("#ad1f1f", "#1f6fad")[clusters$cluster],
     pch = 19)
abline(h = 0)
abline(v = 0)

#Conclusi�n: si bien el gr�fico del codo sugiere que un K <- 3 seria la mejor eleccion,
#al analizar el grafico de siluetas y el grafico de PCA vemos que 3 centro no parecen representar
#de la mejor manera la distribucion de los datos. 
#En base a los graficos de 1) Codo, 2) siluetas, 3) PCA (PC1 Vs PC2) considero que un K <- 2
#es lo que mejor representa la distribucion de los datos.
