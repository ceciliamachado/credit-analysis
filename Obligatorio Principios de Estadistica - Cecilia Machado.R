  # Universidad ORT Uruguay
  # Facultad de Administración y Ciencias Sociales
  # Obligatorio de Principios de Estadística
  # Cecilia Machado - N°213640

  # Paquete de datos requeridos: 
  # ISLR

  # Base de datos:
  # Credit

  # Inicio del preámbulo:

  # Borrar todas las variables de la memoria de trabajo:
  rm(list=ls())
  
  # Establecer carpeta de trabajo:
  setwd("C:/Users/Cecilia/Desktop/ORT/Semestre 2/Principios de Estadística/Obligatorio")
  
  # Cargar el paquete de datos "ISLR"
  library(ISLR)
  
  # Cargar base de datos:
  data("Credit")
  
  # Visualizar base de datos:
  View(Credit)
  
  # Visualizar tipo de datos:
  str(Credit)
  head(Credit)
  
  # Fin del preámbulo 
  
  # Se pide 4a:
  
  # Frecuencias absolutas de la variable Ethnicity
  
  FrecAbs   <- table(Credit$`Ethnicity`)
  FrecAbs
  
  #Frecuencia Absoluta Acumulada:
  
  Frec_Abs_Acum <-cumsum(FrecAbs)
  Frec_Abs_Acum
  
  #Frecuencia Relativa:
  
  RelCredit <- prop.table(FrecAbs)
  RelCredit
  
  #Frecuencia Relativa Acumulada:
  
  Frec_Rel_Acum <-cumsum(RelCredit)
  Frec_Rel_Acum
  
  #Frecuencua Porcentual:
  
  PorCredit <- prop.table(RelCredit)*100
  PorCredit
  
  # Frecuencia Porcentual Acumulada:
  
  Frec_Por_Acum <- cumsum(PorCredit)
  Frec_Por_Acum
  
  # Se combinan en una tabla todas las variables creadas de las distintas frecuencias:
  
  Frecuencias <- cbind(FrecAbs, RelCredit, PorCredit, Frec_Abs_Acum, Frec_Rel_Acum, Frec_Por_Acum)
  Frecuencias
  
  # Confección de gráficos:
  
  # Gráfico de barras de Frecuencias Absolutas
  
  png("Gráfico de barras de Frecuencias Absolutas.png")
  barplot(FrecAbs, main="Gráfico de barras de Frecuencias Absolutas", xlab="Ethnicity",ylab="Frecuencias",ylim=c(0,250))
  dev.off()
  
  # Gráfico circular de Frecuencias Porcentuales:
  
  png("Grafico circular de Frecuencias Porcentuales.png")
  pie(PorCredit,  main="Frecuencias Porcentuales")
  dev.off()
  
  # Se pide 4bi:
  # La primer variable cuantitativa a estudiar será: "Income"
  
  # Tabla de frecuencias agrupadas por clases:
  
  # Ver valores mínimos y máximos de la variable para calcular el ancho de clases:
  
  max(Credit$Income)
  min(Credit$Income)
  
  # Se calcula el ancho de clases de la siguiente manera:
  
  # Ancho de clases = (Valor mayor - Valor menor)/Número de clases
  # Ancho de clases = (186.6 - 10.35)/5 => Ancho de clases = 35.25
  # Se redondea el resultado obtenido, de lo contrario, algunas observaciones quedarían por fuera de las clases.
  # Ancho de clases = 35.5
  # La distribución de frecuencias será: 10.000-45.500, 45.500-81.000, 81.000-116.500, 116.500-152.000, 152.000-188.000.
  
  val_ini         <- 10.000
  val_fin         <- 187.500
  salto           <-  35.500
  clasesIncome          <- seq(val_ini,val_fin,salto)
  clasesIncome
  
  # Se genera una variable tal que cada valor sea a qué clase pertenece cada observación de Income
  clases_Income  <- cut(Credit$Income, breaks = clasesIncome)
  print(clases_Income)
  
  # A esa nueva variable, calcularle las frecuencias absolutas:
  Frec_Abs_Clases_Income      <- table(clases_Income)
  Frec_Abs_Clases_Income
  
  # Expresarlo como un "data frame" que es nuestra tabla deseada a la que le vamos a ir
  # agregando columnas
  
  tabla_frecuencia_clases_income <- data.frame(Frec_Abs_Clases_Income)
  tabla_frecuencia_clases_income
  
  # Agregar al data frame una columna de Frecuencias relativas
  
  Frec_rel        <- tabla_frecuencia_clases_income$Freq/sum(tabla_frecuencia_clases_income$Freq)
  tabla_frecuencia_clases_income$Frec_rel <- Frec_rel
  tabla_frecuencia_clases_income
  
  # Agregar al data frame una columna de Frecuencias porcentuales
  
  Frec_por        <- tabla_frecuencia_clases_income$Freq/sum(tabla_frecuencia_clases_income$Freq)*100
  tabla_frecuencia_clases_income$Frec_por <- Frec_por
  tabla_frecuencia_clases_income
  
  # Agregar al data frame una columna de Frecuencias absolutas acumuladas
  
  Frec_abs_acum   <- cumsum(tabla_frecuencia_clases_income$Freq)
  tabla_frecuencia_clases_income$Frec_abs_acum <- Frec_abs_acum
  print(tabla_frecuencia_clases_income)
  
  # Agregar al data frame una columna de Frecuencias relativas acumuladas
  
  Frec_rel_acum                  <- cumsum(tabla_frecuencia_clases_income$Frec_rel)
  tabla_frecuencia_clases_income$Frec_rel_acum <- Frec_rel_acum
  print(tabla_frecuencia_clases_income)
  
  # Agregar al data frame una columna de Frecuencias porcentuales acumuladas
  
  Frec_por_acum                  <- cumsum(tabla_frecuencia_clases_income$Frec_por)
  tabla_frecuencia_clases_income$Frec_por_acum <- Frec_por_acum
  print(tabla_frecuencia_clases_income)
  
  # La segunda variable cuantitativa a estudiar será: "Rating"
  
  # 1 - Tabla de frecuencias agrupadas por clases:
  
  # Ver valores mínimos y máximos de la variable para calcular el ancho de clases:
  
  max(Credit$Rating)
  min(Credit$Rating)
  
  # Se calcula el ancho de clases de la siguiente manera:
  
  # Ancho de clases = (Valor mayor - Valor menor)/Número de clases
  # Ancho de clases = (982 - 93)/5 => Ancho de clases = 177,8
  # Se redondea el resultado obtenido, de lo contrario, algunas observaciones quedarían por fuera de las clases.
  # Ancho de clases = 180
  # La distribución de frecuencias será: 90-270, 270-450, 450-630, 630-810, 810-990.
  
  val_ini         <- 90
  val_fin         <- 990
  salto           <-  180
  clasesRating          <- seq(val_ini,val_fin,salto)
  clasesRating
  
  # Se genera una variable tal que cada valor sea a qué clase pertenece cada observación de Rating
  clases_Rating  <- cut(Credit$Rating, breaks = clasesRating)
  print(clases_Rating)
  
  # A esa nueva variable, calcularle las frecuencias absolutas:
  Frec_Abs_Clases_Rating      <- table(clases_Rating)
  Frec_Abs_Clases_Rating
  
  # Expresarlo como un "data frame" que es nuestra tabla deseada a la que le vamos a ir
  # agregando columnas
  tabla_frecuencia_clases_Rating <- data.frame(Frec_Abs_Clases_Rating)
  tabla_frecuencia_clases_Rating
  
  # Agregar al data frame una columna de Frecuencias relativas
  
  Frec_rel_Rating        <- tabla_frecuencia_clases_Rating$Freq/sum(tabla_frecuencia_clases_Rating$Freq)
  tabla_frecuencia_clases_Rating$Frec_rel_Rating <- Frec_rel_Rating
  tabla_frecuencia_clases_Rating
  
  # Agregar al data frame una columna de Frecuencias porcentuales
  
  Frec_por_Rating        <- tabla_frecuencia_clases_Rating$Freq/sum(tabla_frecuencia_clases_Rating$Freq)*100
  tabla_frecuencia_clases_Rating$Frec_por_Rating <- Frec_por_Rating
  tabla_frecuencia_clases_Rating
  
  # Agregar al data frame una columna de Frecuencias absolutas acumuladas
  
  Frec_abs_acum_Rating   <- cumsum(tabla_frecuencia_clases_Rating$Freq)
  tabla_frecuencia_clases_Rating$Frec_abs_acum_Rating <- Frec_abs_acum_Rating
  print(tabla_frecuencia_clases_Rating)
  
  # Agregar al data frame una columna de Frecuencias relativas acumuladas
  
  Frec_rel_acum_Rating                  <- cumsum(tabla_frecuencia_clases_Rating$Frec_rel_Rating)
  tabla_frecuencia_clases_Rating$Frec_rel_acum_Rating <- Frec_rel_acum_Rating
  print(tabla_frecuencia_clases_Rating)
  
  # Agregar al data frame una columna de Frecuencias porcentuales acumuladas
  
  Frec_por_acum_Rating                  <- cumsum(tabla_frecuencia_clases_Rating$Frec_por_Rating)
  tabla_frecuencia_clases_Rating$Frec_por_acum_Rating <- Frec_por_acum_Rating
  print(tabla_frecuencia_clases_Rating)
  
  # Se pide 4bii:
  # Se calcula la tendencias centrales (media, mediana y moda) de Income y Rating respectivamente: 
  
  # Tendencia central de Income
  
  # Media
  
  media_Income <- mean(Credit$Income)
  media_Income
  
  # Mediana
  
  mediana_Income <-median(Credit$Income)
  mediana_Income
  
  # Moda
  
  getmode            <- function(v) {
    uniqv            <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  moda               <- getmode(Credit$Income)
  moda
  
  # Tendencia central de Rating
  
  # Media
  
  media_Rating       <-mean(Credit$Rating)
  media_Rating
  
  # Mediana
  
  mediana_Rating     <-median(Credit$Rating)
  mediana_Rating
  
  # Moda
  
  getmode            <- function(v) {
    uniqv            <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  moda               <- getmode(Credit$Rating)
  moda
  
  # Se calcula las medidas de separación (cuartiles y deciles) de Income y Rating respectivamente
  
  # Cuartil de Income
  
  cuartiles_Income          <-  quantile(Credit$Income, c(0.25, 0.5, 0.75))
  cuartiles_Income          <-  as.vector(cuartiles_Income)  
  cuartiles_Income
  
  # Decil de Income
  
  deciles_Income          <-  quantile(Credit$Income, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
  deciles_Income          <-  as.vector(deciles_Income)  
  deciles_Income 
  
  # Cuartil de Rating
  
  cuartiles_Rating          <-  quantile(Credit$Rating, c(0.25, 0.5, 0.75))
  cuartiles_Rating          <-  as.vector(cuartiles_Rating)  
  cuartiles_Rating
  
  # Decil de Rating
  
  deciles_Rating          <-  quantile(Credit$Rating, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
  deciles_Rating          <-  as.vector(deciles_Rating)  
  deciles_Rating 
  
  
  # Se calculan las medidas de disperción (rango o recorrido, Rango intercuartílico, Varianza, Desviación estándar, 
  # coeficiente de variación) de Income y Rating respectivamente.
  
  # Medidas de disperción de Income
  
  # Rango 
  
  range(Credit$Income)
  Rango_Income       <- 186.634-10.354
  Rango_Income
  
  # Rango intercuartílico (Q3 - Q1)
  
  IQR_Income                <- IQR(Credit$Income)
  IQR_Income 
  
  # Varianza
  
  varianza_Income       <- var(Credit$Income)
  varianza_Income
  
  # Desviación estándar
  
  sd_Income                 <-sd(Credit$Income)
  sd_Income
  
  
  # Coeficiente de variación
  
  cv_Income                 <- sd_Income/media_Income
  cv_Income
  
  # Medidas de disperción de Rating
  
  # Rango 
  
  range(Credit$Rating)
  Rango_Rating       <- 982-93
  Rango_Rating
  
  # Rango intercuartílico = Q3 - Q1
  
  IQR_Rating                <- IQR(Credit$Rating)
  IQR_Rating 
  
  # Varianza
  
  varianza_Rating       <- var(Credit$Rating)
  varianza_Rating
  
  # Desviación estándar
  
  sd_Rating                 <-sd(Credit$Rating)
  sd_Rating
  
  # Coeficiente de variación
  
  cv_Rating                 <- sd_Rating/media_Rating
  cv_Rating
  
  #Parte 4b iii:
  
  # Histograma de Income:
  
  hist(Credit$Income, breaks = clasesIncome, xlab = "Income ($)",ylab = "Frecuencia absoluta", main="Histograma de Income", ylim=c(0,300), xlim=c(0,200))
  

  # Histograma de Rating
  
  hist(Credit$Rating, breaks = clasesRating, xlab = "Rating",ylab = "Frecuencia absoluta", main="Histograma de Rating", ylim=c(0,200), xlim=c(0,1000))
  
  # Ojiva de Income:
  
  library(agricolae)
  
  h                 <-hist(Credit$Income, xlab = "Income ($)", ylab = "Frecuencia", main="Ojiva de Income")
  points            <-ogive.freq(h,frame=FALSE, xlab="Income ($)", ylab="Frecuencia relativa acumulada", main="Ojiva de Income")
  print(points)
  
  # Ojiva de Rating:
  
  h                <-hist(Credit$Rating, xlab = "Rating", ylab = "Frecuencia", main="Ojiva de Rating")
  points            <-ogive.freq(h,frame=FALSE, xlab="Income ($)", ylab="Frecuencia relativa acumulada", main="Ojiva de Rating")
  print(points)
  
  #Parte 4b IV:
  
  # Detección de observaciones atípicas:
  
  # Para la variable Income:
  
  # Se definen en variables los datos obtenidos del primer y tercer cuartil:
  
  Q1_Income <- 21.01
  Q1_Income
  
  Q3_Income <- 57.47
  Q3_Income
  
  # Se utiliza nuevamente el Rango intercuartílico:
  
  IQR_Income                <- IQR(Credit$Income)
  IQR_Income 
  
  # Se calcula el limite inferior y superior 
  
  LimInf_Income <- Q1_Income-1.5*IQR_Income
  LimInf_Income
  
  # El valor obtenido es de signo negativo, por lo cual, éste se descarta debido a que no existen números
  # negativos dentro de la variable Income. 
  # Igualmente se continua trabajando con el límite superior. 
  
  LimSup_Income <- Q3_Income+1.5*IQR_Income
  LimSup_Income
  
  # Se obtienen los índices que están arriba del límite superior:
  
  OutliersSup_Income <- which(Credit$Income>LimSup_Income)
  OutliersSup_Income
  
  # Se ultiliza la siguiente función para mostrar los valores de los datos atípicos:
  
  boxplot.stats(Credit$Income)[4]
  
  # El número de elementos de OutliersSup_Income se corresponde 
  # con el número de outliers del límite superior.
  
  length(OutliersSup_Income)
  
  
  # Para la variable Rating:
  
  # Se definen en variables los datos obtenidos del primer y tercer cuartil:
  
  Q1_Rating <- 247.2
  Q1_Rating
  
  Q3_Rating <- 437.2
  Q3_Rating
  
  # Se utiliza nuevamente el Rango intercuartílico:
  
  IQR_Rating                <- IQR(Credit$Rating)
  IQR_Rating 
  
  # Se calcula el limite inferior y superior 
  
  LimInf_Rating <- Q1_Rating-1.5*IQR_Rating
  LimInf_Rating
  
  # El valor obtenido es de signo negativo, por lo cual, éste se descarta debido a que no existen números
  # negativos dentro de la variable Income. 
  # Igualmente se continua trabajando con el límite superior. 
  
  LimSup_Rating <- Q3_Rating+1.5*IQR_Rating
  LimSup_Rating
  
  # Se obtienen los datos que están arriba de los límites superiores e inferiores:
  
  OutliersSup_Rating <- which(Credit$Rating>LimSup_Rating)
  OutliersSup_Rating
  
  # Se ultiliza la siguiente función para mostrar los valores de los datos atípicos:
  
  boxplot.stats(Credit$Rating)[4]
  
  # El número de elementos de OutliersSup_Income se corresponde 
  # con el número de outliers del límite superior.
  
  length(OutliersSup_Rating)

  # Se pide 5:
  
  # Diagrama de caja de Income:
  
  boxplot(Credit$Income, main = "Diagrama de caja de Income", ylab="Income($)" )
  
  # Diagrama de caja de Rating:
  
  boxplot(Credit$Rating, main = "Diagrama de caja de Rating", ylab="Rating" )
  
  # Diagrama de caja de Income/Ethnicity
  
  boxplot(Credit$Income ~ Credit$Ethnicity, data = Credit, "Income", main = "Diagrama de cajas Income/Ethnicity", xlab="Ethnicity", ylab="Income")
  
  # Diagrama de caja de Rating/Ethnicity
  
  boxplot(Credit$Rating ~ Credit$Ethnicity, data = Credit, "Rating", main = "Diagrama de cajas Rating/Ethnicity", xlab="Ethnicity", ylab="Rating")

  # Se pide 6: Análisis de asociación entre Income y Ethnicity
  
  # Se crea tabulación cruzada de datos:
  
  tablaEhtnicityInome <- table(Credit$Ethnicity, clases_Income)
  tablaEhtnicityInome
  
  # Se agregan los totales (distribuciones marginales)
  tablaEhtnicityInome <- addmargins(tablaEhtnicityInome)  
  tablaEhtnicityInome
  
  # Se elabora distribución de frecuencia porcentual
  
  options("digits"=1)               #defino un decimal 
  
  tabla2 =  tablaEhtnicityInome[,-6]
  tabla2
  
  tabla_fila = prop.table(tabla2,1)  # la opcion 1 es porcentaje por fila y la 2 por columna
  tabla_fila
  tabla_fila = addmargins(tabla_fila) 
  tabla_fila = tabla_fila[-5,]
  
  
  tabla_fila = tabla_fila*100
  tabla_fila
  
  # Para visualizar solo la fila del total porcentual realizo la siguiente sentencia:
  
  frec_income = as.matrix(tabla_fila[4,])
  colnames(frec_income) = "Frec. Porcentual"
  frec_income
  
  # Cálculo de Chi Cuadrado e índice de Cramer
  
  tablaEhtnicityInome = tablaEhtnicityInome[-4,-6]
  tablaEhtnicityInome
  
  options("digits"=4)               #defino un decimal 
  
  chi.cuad = chisq.test(tablaEhtnicityInome, simulate.p.value = TRUE)
  chi.cuad
  
  chi.cuad = chi.cuad$statistic
  chi.cuad
  
  cramer = sqrt(chi.cuad/(sum(tablaEhtnicityInome)*2))
  cramer
  
 
  # Se pide 7: Una comparación de las distribuciones de las dos variables 
  # cuantitativas seleccionadas mediante un diagrama de dispersión
  
  
  plot(Credit$Income, Credit$Rating, xlab="Income", ylab="Rating", 
       main="Diagrama de dispersión") 
  
  
  # agrego la recta de regresion al diagrama de dispersion
  
  reg = lm(Rating~Income, Credit) 
  reg
  abline(reg) 
  
  # Calculo de la covarianza
  cov(Credit$Income, Credit$Rating)
  
  # Calculamos coeficiente de correlación lineal de Pearson entre las variables
  cor(Credit$Income, Credit$Rating)
  
  # Comprobamos que hay correlación positiva y por lo tanto las variables están relacionadas
  
  
  # BONUS
  
  # La variable cualitativa es Ethnicity
  # Se pide un análisis de la distribución por grupos de cada variable cuantitativa 
  # por clases de la variable cualitativa.

  
  with(Credit,
       plot(Credit$Rating, Credit$Income,
            pch=as.numeric(Credit$Ethnicity), cex=1.2, ylab="Rating", xlab="Income"))
  
  legend( x= "bottomright",
          legend=c("African-American", "Caucasian", "Asian"), 
          pch=as.numeric(Credit$Ethnicity), cex=0.8)
  
