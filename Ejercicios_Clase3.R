# a) Cargar el dataset antropometria
dir() 

antro <- read.csv("antropometria_ok.csv")



# b) Analizar la estructura del dataset.
#Ayuda: ?class, ?str, ?colname. ?nrow, ?ncol, ?head, ?tail

class(antro)
str(antro)
head(antro)
tail(antro)

# c) ¿Cu´antos hombres y cu´antas mujeres tiene el dataset? Ayuda: ?table

unique(antro$sex)
table(antro$sex)

# d) Calcular m´aximo, m´inimo, media, mediana, varianza y desviaci´on est´andar 
#de las alturas separadas por sexo. ¿Cu´al tiene mayor media? ¿Y mayor varianza?
# Ayuda: ?max, ?min, ?mean, ?median, ?var, ?sd, ?summary



mean(antro$height)

# FUNCION AGGREGATE############### SUPER IMPORTANTE!!!
# es forma de analizar datos por grupos es genial!!
# x~y significa x~separado por Y o x agrupado por Y
aggregate(data=antro, height ~ sex, FUN = mean)
aggregate(data=antro, height ~ sex, FUN = sd)
aggregate(data=antro, height ~ sex, FUN = summary)
aggregate(data=antro, height ~ sex, FUN = max)
aggregate(data=antro, height ~ sex, FUN = min)

aggregate(data=antro, weight ~ sex, FUN = mean)
aggregate(data=antro, weight ~ sex, FUN = sd)
aggregate(data=antro, weight ~ sex, FUN = summary)
aggregate(data=antro, weight ~ sex, FUN = max)
aggregate(data=antro, weight ~ sex, FUN = min)



#e) Cargar el dataset cases-covid-19-by-country.csv de una forma apropiada.

covid <- read.csv("WHO-COVID-19-global-data.csv")
head(covid)
colnames(covid)
View(covid)
any(is.na(covid))
covid <- covid[complete.cases(covid),]


unique(covid$Country)
length(unique(covid$Country))

# con esta linea podemos ver cual fue el numero de casos totales reportados

df <- aggregate(data=covid, Cumulative_cases ~ Country, FUN = max)
# creo que no encontre la tabla que nos proponian usar por lo tanto esta es la manera
# que encontre de armar una table que tuviera el numero total de casos agrupados por pais
View(df)


# f) ¿Cu´antos datos u observaciones contiene el dataset? 
# ¿Cu´antos atributos tiene cada dato?
class(dim(covid))
length(dim(covid))
obs <- dim(covid)[1]
cols <- dim(covid)[2]

cat("El DataSet contiene", obs, "observaciones")

# g) ¿De qu´e tipo son los atributos? ¿C´omo se llaman?
str(covid)


# h) Mostrar los primeros 5 datos y los ´ultimos 5 datos.
head(covid)
tail(covid)

# i) Realizar una descripci´on general del dataset a partir de 
# este an´alisis con tus palabras
num_countries <- length(unique(covid$Country_code))


cat("El data set contiene", obs, "observaciones. Los datos estan divididos en", cols,"columnas.") 
cat("Hay datos de", num_countries, "paises diferentes.")


View(covid)


# j) Calcular la media, la mediana y la moda para las variables num´ericas.
# ¿Son similares o muy diferentes?

summary(df)
median(df$Cumulative_cases)

# k) Calcular el rango, el desv´io estandar y el IQR.
#A partir de esto indicar si los valores est´an muy concentrados o si por el contrario, est´an muy
#dispersos.


IQR(df$Cumulative_cases)
sd(df$Cumulative_cases)



# l) En base a lo visto anteriormente, ¿Qu´e medidas usar´ias para resumir los valores?


cat("Media =", mean(df$Cumulative_cases),
    "\nMediana = ", median(df$Cumulative_cases),
    "\nDesviacion estandar =", sd(df$Cumulative_cases),
    "\nIQR =", IQR(df$Cumulative_cases))

#############################################################################
####################### 2. Visualizaci´on ###################################

# a) Cargar el dataset antropometria

antro <- read.csv("antropometria_ok.csv")



# b) ¿Existe relaci´on entre la altura y la edad? Graficar en un scatter plot altura vs. edad y decidir.
# ¿Hubiera cambiado la conclusi´on de haber realizado el mismo gr´afico separado por sexo? Ayuda:
#  ?plot


plot(x = antro$age,
     y = antro$height,
     xlab = "Edad",
     ylab = "Altura (cm)",
     pch = 19,
     main = "Altura Vs Edad"
     )


# -----------------------------------------------------------------------------
hombres <- antro[antro$sex == 'M',]
View(hombres)
mujeres <- antro[antro$sex == 'F',]
View(mujeres)

layout(matrix(1:2, ncol = 2))
plot(x = hombres$age,
     y = hombres$height,
     xlab = "Edad",
     ylab = "Altura (cm)",
     pch = 19,
     col = "#020085",
     main = "Hombres"
)

plot(x = mujeres$age,
     y = mujeres$height,
     xlab = "Edad",
     ylab = "Altura (cm)",
     pch = 19,
     col = "#850000",
     main = "Mujeres"
)
layout(1)
# -----------------------------------------------------------------------------

plot(x = hombres$age,
     y = hombres$height,
     xlab = "Edad",
     ylab = "Altura (cm)",
     pch = 19,
     col = "#020085",
     main = "Edad Vs Altura"
)

points(x = mujeres$age, #esta funcion nos permite superponer estos datos al grafico que
     y = mujeres$height, # creamos mas arriba.
     xlab = "Edad",
     ylab = "Altura (cm)",
     pch = 19,
     col = "#850000",
     )
legend("bottomright",legend=c("Hombres","Mujeres"),col=c("#020085","#850000"),pch = 19)


# c) Agregar una recta vertical en rojo en donde comienza el plateau. Ayuda: ?abline

abline(v = 20,
       col = "#005d85",
       lwd = 3,
       lty = 2)


# d) Para remover el efecto de la edad, utilizar el gr´afico del punto anterior para decidir a qu´e edad la
# misma deja de tener efecto en la altura y subsetear el dataset original en un vector nuevo llamado
# "adultos".

# elegimos medio a ojo el valor de edad a partir del cual no se observa que influya sobre la altura
# ponemos el valor de 20

adulto <- antro[antro$age>20,]
plot(x = adulto$age,
     y = adulto$height,
     ylim = 
     xlab = "Edad",
     ylab = "Altura (cm)",
     pch = 19,
     col = "#98eb34",
     main = "Edad Vs Altura \n ADULTOS"
)




layout(matrix(1:2,ncol = 2))
hist(hombres$height,
     freq = F)
hist(mujeres$height,
     freq = F)
