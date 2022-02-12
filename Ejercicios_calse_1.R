# EJERCICIOS CLASE 1

# 1) Asignale a la variable x el valor 2 y a la variable y el valor 10. Ahora asignale a la variable
# w el valor x+y y a la variable z el valor x == y (observá que tiene dos signos = y no sólo                                                  uno, ¿Por qué?). ¿De qué clase son estas dos últimas variable?. Ayuda: la asignación de una
# variable se realiza con el operador "<-", así por ejemplo para asignar el valor "Hola Mundo"
# a la variable Saludo debo tipear Saludo<-"Hola Mundo". Además, tipeá en la consola ?class

# alt + - para hacer la flechita que asigna valor


x <- 2
y <- 10



w <- x + y

z <- x == y # esto nos da como resultado un FALSE

saludo <- 'Hola Mundo'

?class


class(saludo)
class(z)
class(w)


# 2) Creá una variable que se llame "GFP" y asignale el valor 509. ¿Qué clase de dato es? Ahora
# convierte la variable "GFP" en "character". Ayuda: tipeá en la consola ?as.character


GFP <- 509
class(GFP)
GFPas.character(GFP)
class(GFP)


# 4) Crea un vector que se llame "r1" que contenga 10 números aleatorios elegidos por vos entre
# el 0 y el 100 Ayuda: ?c.

r1 <- c( 22, 33, 34, 23, 16, 78, 45, 67, 70, 12)
?c # c significa combine y sirve para armar listas o vectores

# 5) Con la misma operación que en el ejercicio anterior crea un nuevo vector r2, ahora realiza
# las siguientes operaciones: r1+r2, r1*r2, r1/r2. ¿Cómo realiza R estas operaciones?

r2 <-   c( 0, 90, 89, 51, 11, 21, 65, 40, 13, 33)

r1 + r2

r1 * r2

div <- r1/r2

div
# la division por cero nos va a dar inf (infinito)


# 6) Intenten ordenar de mayor a menor y de menor a mayor el vector r1. ¿De qué largo es el
# vector r1?¿Cómo podría agregarle un elemento al vector r1? Ayuda: ?sort, ?length

?sort
sort(r1, decreasing=FALSE)
sort(r1, decreasing=TRUE)




r3 <- c("hola", "chau", "dia")
sort(r3, decreasing = TRUE)

?length

r3 <- c(r1, 'juan') # la C viene de "CONCATENAR"


r3

r5 <- c(r1, r3)
r5
class(r5)
length(r3)

r3
r3[1,-1]

r3[4] <- 'nico'
r3


------------------------------------------------------------------------------------------
  #7) Creá dos vectores. Uno llamado fp que contenga los siguientes datos: "Sirius", "CFP",
  # "GFP", "Citrine", y otro llamado nm que contenga los siguientes valores: 424, 476, 509,
  # 528. A partir de estos dos vectores crea una matriz m_fp y un dataframe que se llame df_fp.
  # Comprueba de qué clase es cada objeto. ¿Qué tienen de diferente? Ayuda: ?cbind y ?
  #  data.frame
  
  
  
  fp <- c("Sirius", "CFP", "GFP", "Citrine") 
nm <- c(424, 476, 509, 528)
m_fp <- cbind(fp, nm) # agrega los datos en columnas
rbind(fp, nm) # agrega los datos en filas
# notar que se oonvierte todo a character


?data.frame
# las dataframes nos permiten que cada culuman tenga tipos de datos diferentes. Eso no se podia hacer con
# los cbind o rbind

df_fp <- data.frame(fp, nm)
df_fp

str(df_fp) # para averiguar que datos y que tipo de datos hay en cada columna



#-------------------------------------------------------------------------------
# 8) Construí un vector con todos los números del 1 al 9. Ayuda: comienzo:fin

vector <- 1:9
vector

vector_2 <- seq(from=10,to=90, by=2)
vector_2



#-------------------------------------------------------------------------------
# 10) Crea una matriz m de 3x2 (3 filas y 2 columnas) que contenga en todas
# las posiciones el valor 0. Ayuda: ?matrix

?matrix
matriz_1 <- matrix(data = 0,
                   nrow = 3,
                   ncol = 2
                   )


matriz_1

# filas_columnas <- list(list('Columna 1', 'Columna 2'), list('Fila 1', 'Fila 2'))


matriz_2 <- matrix(data = 0,
                   nrow = 3,
                   ncol = 2,
#                   dimnames = filas_columnas
)

matriz_2


#------------------------------------------------------------------------------
# 11) Crea una matriz m de 3x3 que contenga en la posición [1, 1] el número 1, en la [1, 2] el
# número 2 y así hasta el número 9 en la posición [3, 3]. Los elementos de una matriz son
# [fila, columna]. Ayuda: ?matrix y ejercicio 8).


data_11 <- seq(from = 1, to = 9)
data_11
matriz_11 <- matrix(nrow = 3,
                    ncol = 3,
                    data = data_11,
                    byrow = TRUE
                    )
matriz_11


#------------------------------------------------------------------------------
# 12) A la matriz del punto anterior cambiale el valor de la primera fila y columna por "Esto es un
# string" con el siguiente código: m[1, 1] <- "Esto es un string" (¿Podrías explicar cómo
# funciona esto?). Imprimir el resultado en pantalla, ¿Qué ocurrió con el resto de los valores
# de la matriz? ¿Por qué?


class(matriz_11)
class(matriz_11[1,1])
matriz_11[1,1] <- 'Esto es un string'
class(matriz_11[1,1])
matriz_11
class(matriz_11)
# notar que el resto de los elementos de la matriz ahora tambi'en son strings (characters)





#------------------------------------------------------------------------------
# 13) Solo si llegamos a ver factores en clase: a partir de un vector "Co2" con los siguientes
# elementos: "Bajo", "Medio", "Medio", "Bajo", "Bajo" y "Alto"; crea un objeto de tipo
# factor. ¿Qué niveles (o levels) tiene? Ahora cambia el primer valor del objeto por "Medio"
# de la siguiente forma Co2[1] <- "Medio" ¿Cambiaron los niveles del objeto Co2? Ahora si
# quiero cambiar el primer valor por "Muy bajo", ¿Qué ocurre? ¿Por qué? Ayuda: ?factor, ?
#  levels

Co2 <- c('Bajo', 'Medio', 'Medio', 'Bajo', 'Bajo', 'Alto')
class(Co2)
Co2

factor(Co2)


Co2
levels(Co2)
?levels
Co2[1]  <-  'Medio'


Co2[1]  <-  'Muy bajo'

Co2

################################################################################
# 14) Generen dos listas a partir de las siguientes sintaxis: nombres1 <- list(c("Juana", "Pedro",
# "Camila")) y nombres2 <- list("Juana", "Pedro", "Camila"). ¿Cuántos elementos tienen cada
# una de ellas? Para cada caso intenten imprimir en pantalla solamente el nombre Camila

nombres1 <- list(c('Juana', 'Pedro', 'Camila'))
nombres2 <- list('Juana', 'Pedro', 'Camila')

length(nombres1)
length(nombres2)
nombres1[[1]][3]
nombres2[[3]]
nombres2[3]

###############################################################################
#A partir del paquete "datasets" generen un data.frame "air" que contiene datos climáticos de la
#ciudad de Nueva York, usando el siguiente comando: air <- datasets::airquality. (Hay algunos
#paquetes precargados en R, como por ejemplo datasets, a los que se accede poniendo nombre del
#paquete::).

air <- datasets::airquality

dim(air)
colnames(air)


################################################################################
# 16) Imprime en pantalla todos los valores de Temperaturas registradas y luego solamente el
# valor de Temperatura que se registró el tercer día.

# hay, al menos 3 formas diferentes de hacer esto
air$Temp
print(air$Temp)
air[,4]
air[,"Temp"]

air[3,4]

###############################################################################
# 17) Seleccioná todas las filas de air del mes de mayo

class(air)
class(air$Month)
levels(air$Month)

mayo <- which(air$Month == 5)
mayo

View(air)

air[which(air$Month == 5), ]


mes_mayo <- air[which(air$Month == 5), ]
mes_mayo

##############################################################################
# 18) ¿En qué día hubo menor radiación solar? ¿De cuánto fue esa radiación? Ayuda: ?which.min
# y ?min. Al correr el comando min, observamos que el resultado devuelto es NA. ¿Qué
# significa? ¿Por qué es el mínimo?. Probar nuevamente con min pero pasando el parámetro
# na.rm = TRUE, es decir, min(vector, na.rm=TRUE). ¿Qué obtenemos ahora?

colnames(air)

min(air$Solar.R)
# notar que el resultado de aplicar esta funcion da NA. Tengo dos opciones: eliminar las filas que tengan
# datos NA o aplicar un argumento en la funcion min() que hace que no se tengan en cuenta los NA

min(air$Solar.R, na.rm = T)

##############################################################################
# 19) ¿Cuál fue la temperatura el 27 de agosto? Ayuda: ?which, & (este símbolo se llama
# ampersand y es equivalente a "y". Permite concatenar dos condiciones, por ejemplo, quiero
#que el día sea el 27 y el mes sea agosto. Solo aquellos registros que cumplan ambas
#condiciones van a ser incluidos.)

air$Month == 5
air$Day == 27

which(air$Month == 5) # esto me da el indice de las fila en las que hay Month == 5
which(air$Day == 27) # esto me da el indice de las filas en las que hay Day == 27

mayo_27 <- air[which(air$Month == 5 & air$Day == 27),"Temp"]
mayo_27

?cat()

cat('La temperatura del 27 de Mayo fue', mayo_27, '\bF.')


################################################################################
# 20) ¿Cuál fue la temperatura los meses de agosto y septiembre? Ayuda: ?which, | (este símbolo
#se llama pipe o tubería y es equivalente a "o". Permite concatenar dos condiciones, por
#ejemplo, quiero que el mes sea agosto o el mes sea septiembre. Solo aquellos registros que
#cumplan alguna de las dos condiciones van a ser incluidos.)

air$Month == 8 # R evalua esta condicion de verdad en cada celda de la columna Month. Devuelve T o F

which(air$Month == 8) # Devuelve una lista de numeros. Son los indice de las filas en las que la condicion
                      # de verdad que se evalua es verdadera

air[which(air$Month == 8),] # Devuelve una DF. Que contien las filas seleccionadas y todas las columnas

air[which(air$Month == 8),"Temp"] # Devuelve una lista. Que contien las filas seleccionadas y la columna "Temp"

temp_agosto <- air[which(air$Month == 8),"Temp"]

temp_septiembre <- air[which(air$Month == 9),"Temp"]


cat('La temperatura durante el mes de agosto fue\n', temp_agosto)

cat('La temperatura durante el mes de septiembre fue\n', temp_septiembre)

cat('La temperatura durante el mes de agosto fue\n', temp_agosto,
    '\nLa temperatura durante el mes de septiembre fue\n', temp_septiembre
    ) # imprimo todo junto las lineas de mas arriba

temp_ago_sep <- air[which(air$Month == 8 | air$Month == 9),"Temp"]
temp_ago_sep



###############################################################################
# 21) Seleccioná todas las filas de air del mes de mayo cuya radiación solar sea mayor a 150

may_HighRad <- air[which(air$Month == 5 & air$Solar.R > 150),]
may_HighRad
View(may_HighRad)


###############################################################################
# 22) La temperatura en el data.frame está expresada en grados Fahrenheit, generá una nueva
# columna que se llame temp_c con las temperaturas en grados Celsius. Ayuda: °C = (°F -32)/1.8

air$newcol <- NA
View(air)

# forma de agregar una columna usando a otra columna para hacer un calculo
air$newcol2 <- air$Temp + 1000   
View(air)

air$"Grados Celsius" <- (air$Temp - 32)/1.8 
View(air)

# el resultado de convertir a C me da nueros decimales con muchos digitos despues de la coma
# voy a redondear para que quede solo un digito
?round()
air$"Grados Celsius" <- round(air$"Grados Celsius", digits = 1)
View(air)


################################################################################
# 23) Genera un nuevo data.frame llamado "calor" que contenga la información de los días
#calurosos, por ejemplo, en los que hizo más de 30°C. Luego, utilizando la función table
#contá cuantos días calurosos hubo en cada mes. ¿Qué mes es el que tuvo mayor cantidad de
#días calurosos?. Ayuda: La función table permite contar casos. Por ejemplo, aplicando table
#al vector meses <- c("julio", "julio", "agosto", "marzo", "agosto", "julio"), obtenemos la
#siguiente tabla:



which(air$"Grados Celsius" > 30)

calor <- air[which(air$"Grados Celsius" > 30), ]
View(calor)



meses <- c("julio", "julio", "agosto", "marzo", "agosto", "julio")

table(meses) #la funcion table se aplica a un vector, por lo tanto tengo que aplicarla a una columna de la DF

table(calor$Month)


###################### FIN ###############################################

# supongamos que quisiera reemplazar los numeros de los meses por los nombres...