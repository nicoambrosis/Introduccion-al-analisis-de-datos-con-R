# Importación, limpieza y exportación de datos


#1)	Importar el dataset "antropometria" de alturas, pesos, edades y sexos de una muestra poblacional. 
#¿Qué tipo de datos contiene el dataframe? ¿Existen elementos con NA? 
#Generar una nueva tabla removiendo todas las filas con algún NA y exportarla a un 
#nuevo csv llamado antropometria_filtrado.csv


antropometria <- read.csv("antropometria.csv")
View(antropometria)

colnames(antropometria)
summary(antropometria$height)
summary(antropometria$weight)
summary(antropometria$age)


str(antropometria)

summary(antropometria)

#hay NA's
any(is.na(antropometria))


#table(is.na(antropo$height))

complete.cases(antropometria) # T cuando no hay NA, F cuando hay NA


antropometria <- antropometria[complete.cases(antropometria),]




write.csv(antropometria,
          file = "antropometria_ok.csv",
          quote = F,
          row.names = F)


# 2)	Durante la pandemia se realizó un estudio sobre los posibles beneficios que podrían tener las técnicas de mindfulness para disminuir los niveles de depresión y ansiedad en estudiantes de la universidad de Oxford. Con este objetivo se dieron los niveles de depresión y ansiedad en tres tiempos: antes del comienzo del comienzo del curso de mindfulness (T1), inmediatamente luego de terminarlo (T2) y un mes más tarde (T3)
#a.	Importe la tabla oxford_mindfulness.xlsx.
#b.	Haga una limpieza de los datos: identifique datos faltantes, filas duplicadas, datos anómalos. Elimine las filas que presenten estos problemas.
#c.	Agregue a la tabla la información del cambio en los niveles de depresión y ansiedad entre el inicio y el final del estudio. 
#Ejemplo: Depression_T3T1 = Depression_T3 - Depression_T1
#d.	Exporte el dataset en formato xlsx.

# a.
install.packages("readxl")
library("readxl")


??read.xlsx
mindfulness <- read_excel("oxford_mindfulness.xlsx")
View(mindfulness)
colnames(mindfulness)

# b.
any(is.na(mindfulness))

mindfulness <- mindfulness[complete.cases(mindfulness),]

any(duplicated(mindfulness))

# hay datos anomalos? Para esto voy a usar la funcion unique() aplicada a cada columna.

unique(mindfulness$Reference_Number)
unique(mindfulness$Age)
unique(mindfulness$Gender)
unique(mindfulness$Citizen)
unique(mindfulness$Ethnicity)
unique(mindfulness$English)
unique(mindfulness$Degree)
unique(mindfulness$Extraversion)
unique(mindfulness$Agreeable)
unique(mindfulness$Conscientious)
unique(mindfulness$Neuroticism)
unique(mindfulness$Openness)
unique(mindfulness$Meditation)
unique(mindfulness$Mindfulness)
unique(mindfulness$Motivation)
unique(mindfulness$Brexit_Identity)
unique(mindfulness$Identity_Strength)
unique(mindfulness$Treatment)
unique(mindfulness$Depression_T1)
unique(mindfulness$Depression_T2)
unique(mindfulness$Depression_T3)
unique(mindfulness$Anxious_T1)
unique(mindfulness$Anxious_T2)
unique(mindfulness$Anxious_T3)
# yo no veo datos raros

# c. #c.	Agregue a la tabla la información del cambio en los niveles de depresión y 
#ansiedad entre el inicio y el final del estudio. 
#Ejemplo: Depression_T3T1 = Depression_T3 - Depression_T1


colnames(mindfulness)

mindfulness$"Depression_T3T1" <- (mindfulness$"Depression_T3" - mindfulness$"Depression_T1") 
View(mindfulness)  


summary(mindfulness$Depression_T1)  

summary(mindfulness$Depression_T3)  

summary(mindfulness$Depression_T3T1)  


mindfulness$"Anxious_T3T1" <- (mindfulness$"Anxious_T3" - mindfulness$"Anxious_T1") 
View(mindfulness)

summary(mindfulness$Anxious_T1)  

summary(mindfulness$Anxious_T3)  

summary(mindfulness$Anxious_T3T1)  


write.xlsx(mindfulness, file = "oxford_mindfulness_ok.xlsx", row.names = FALSE)


