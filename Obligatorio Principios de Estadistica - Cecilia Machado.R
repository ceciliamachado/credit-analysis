  # Universidad ORT Uruguay
  # Facultad de Administracion y Ciencias Sociales
  # Obligatorio de Principios de Estadistica
  # Cecilia Machado

  # Required packages: 
  # ISLR

  # Database:
  # Credit

  #***************
  # Initalization:
  #***************

  # Delete all variables from memory:
  rm(list=ls())
  
  # Set up working directory:
  setwd("C:/Users/Cecilia Machado/Documents/GitHub/credit")
  
  # Load data from package "ISLR"
  library(ISLR)
  
  # Load database:
  data("Credit")
  
  # View database:
  View(Credit)
  
  # Visualizar tipo de datos:
  str(Credit)
  head(Credit)
  
  # Fin del pre�mbulo 
  
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
  
  # Confecci�n de gr�ficos:
  
  # Gr�fico de barras de Frecuencias Absolutas
  
  png("Gr�fico de barras de Frecuencias Absolutas.png")
  barplot(FrecAbs, main="Gr�fico de barras de Frecuencias Absolutas", xlab="Ethnicity",ylab="Frecuencias",ylim=c(0,250))
  dev.off()
  
  # Gr�fico circular de Frecuencias Porcentuales:
  
  png("Grafico circular de Frecuencias Porcentuales.png")
  pie(PorCredit,  main="Frecuencias Porcentuales")
  dev.off()
  
  # Se pide 4bi:
  # La primer variable cuantitativa a estudiar ser�: "Income"
  
  # Tabla de frecuencias agrupadas por clases:
  
  # Ver valores m�nimos y m�ximos de la variable para calcular el ancho de clases:
  
  max(Credit$Income)
  min(Credit$Income)
  
  # Se calcula el ancho de clases de la siguiente manera:
  
  # Ancho de clases = (Valor mayor - Valor menor)/N�mero de clases
  # Ancho de clases = (186.6 - 10.35)/5 => Ancho de clases = 35.25
  # Se redondea el resultado obtenido, de lo contrario, algunas observaciones quedar�an por fuera de las clases.
  # Ancho de clases = 35.5
  # La distribuci�n de frecuencias ser�: 10.000-45.500, 45.500-81.000, 81.000-116.500, 116.500-152.000, 152.000-188.000.
  
  val_ini         <- 10.000
  val_fin         <- 187.500
  salto           <-  35.500
  clasesIncome          <- seq(val_ini,val_fin,salto)
  clasesIncome
  
  # Se genera una variable tal que cada valor sea a qu� clase pertenece cada observaci�n de Income
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
  
  # La segunda variable cuantitativa a estudiar ser�: "Rating"
  
  # 1 - Tabla de frecuencias agrupadas por clases:
  
  # Ver valores m�nimos y m�ximos de la variable para calcular el ancho de clases:
  
  max(Credit$Rating)
  min(Credit$Rating)
  
  # Se calcula el ancho de clases de la siguiente manera:
  
  # Ancho de clases = (Valor mayor - Valor menor)/N�mero de clases
  # Ancho de clases = (982 - 93)/5 => Ancho de clases = 177,8
  # Se redondea el resultado obtenido, de lo contrario, algunas observaciones quedar�an por fuera de las clases.
  # Ancho de clases = 180
  # La distribuci�n de frecuencias ser�: 90-270, 270-450, 450-630, 630-810, 810-990.
  
  val_ini         <- 90
  val_fin         <- 990
  salto           <-  180
  clasesRating          <- seq(val_ini,val_fin,salto)
  clasesRating
  
  # Se genera una variable tal que cada valor sea a qu� clase pertenece cada observaci�n de Rating
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
  
  # Se calcula las medidas de separaci�n (cuartiles y deciles) de Income y Rating respectivamente
  
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
  
  
  # Se calculan las medidas de disperci�n (rango o recorrido, Rango intercuart�lico, Varianza, Desviaci�n est�ndar, 
  # coeficiente de variaci�n) de Income y Rating respectivamente.
  
  # Medidas de disperci�n de Income
  
  # Rango 
  
  range(Credit$Income)
  Rango_Income       <- 186.634-10.354
  Rango_Income
  
  # Rango intercuart�lico (Q3 - Q1)
  
  IQR_Income                <- IQR(Credit$Income)
  IQR_Income 
  
  # Varianza
  
  varianza_Income       <- var(Credit$Income)
  varianza_Income
  
  # Desviaci�n est�ndar
  
  sd_Income                 <-sd(Credit$Income)
  sd_Income
  
  
  # Coeficiente de variaci�n
  
  cv_Income                 <- sd_Income/media_Income
  cv_Income
  
  # Medidas de disperci�n de Rating
  
  # Rango 
  
  range(Credit$Rating)
  Rango_Rating       <- 982-93
  Rango_Rating
  
  # Rango intercuart�lico = Q3 - Q1
  
  IQR_Rating                <- IQR(Credit$Rating)
  IQR_Rating 
  
  # Varianza
  
  varianza_Rating       <- var(Credit$Rating)
  varianza_Rating
  
  # Desviaci�n est�ndar
  
  sd_Rating                 <-sd(Credit$Rating)
  sd_Rating
  
  # Coeficiente de variaci�n
  
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
  
  # Detecci�n de observaciones at�picas:
  
  # Para la variable Income:
  
  # Se definen en variables los datos obtenidos del primer y tercer cuartil:
  
  Q1_Income <- 21.01
  Q1_Income
  
  Q3_Income <- 57.47
  Q3_Income
  
  # Se utiliza nuevamente el Rango intercuart�lico:
  
  IQR_Income                <- IQR(Credit$Income)
  IQR_Income 
  
  # Se calcula el limite inferior y superior 
  
  LimInf_Income <- Q1_Income-1.5*IQR_Income
  LimInf_Income
  
  # El valor obtenido es de signo negativo, por lo cual, �ste se descarta debido a que no existen n�meros
  # negativos dentro de la variable Income. 
  # Igualmente se continua trabajando con el l�mite superior. 
  
  LimSup_Income <- Q3_Income+1.5*IQR_Income
  LimSup_Income
  
  # Se obtienen los �ndices que est�n arriba del l�mite superior:
  
  OutliersSup_Income <- which(Credit$Income>LimSup_Income)
  OutliersSup_Income
  
  # Se ultiliza la siguiente funci�n para mostrar los valores de los datos at�picos:
  
  boxplot.stats(Credit$Income)[4]
  
  # El n�mero de elementos de OutliersSup_Income se corresponde 
  # con el n�mero de outliers del l�mite superior.
  
  length(OutliersSup_Income)
  
  
  # Para la variable Rating:
  
  # Se definen en variables los datos obtenidos del primer y tercer cuartil:
  
  Q1_Rating <- 247.2
  Q1_Rating
  
  Q3_Rating <- 437.2
  Q3_Rating
  
  # Se utiliza nuevamente el Rango intercuart�lico:
  
  IQR_Rating                <- IQR(Credit$Rating)
  IQR_Rating 
  
  # Se calcula el limite inferior y superior 
  
  LimInf_Rating <- Q1_Rating-1.5*IQR_Rating
  LimInf_Rating
  
  # El valor obtenido es de signo negativo, por lo cual, �ste se descarta debido a que no existen n�meros
  # negativos dentro de la variable Income. 
  # Igualmente se continua trabajando con el l�mite superior. 
  
  LimSup_Rating <- Q3_Rating+1.5*IQR_Rating
  LimSup_Rating
  
  # Se obtienen los datos que est�n arriba de los l�mites superiores e inferiores:
  
  OutliersSup_Rating <- which(Credit$Rating>LimSup_Rating)
  OutliersSup_Rating
  
  # Se ultiliza la siguiente funci�n para mostrar los valores de los datos at�picos:
  
  boxplot.stats(Credit$Rating)[4]
  
  # El n�mero de elementos de OutliersSup_Income se corresponde 
  # con el n�mero de outliers del l�mite superior.
  
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

  # Se pide 6: An�lisis de asociaci�n entre Income y Ethnicity
  
  # Se crea tabulaci�n cruzada de datos:
  
  tablaEhtnicityInome <- table(Credit$Ethnicity, clases_Income)
  tablaEhtnicityInome
  
  # Se agregan los totales (distribuciones marginales)
  tablaEhtnicityInome <- addmargins(tablaEhtnicityInome)  
  tablaEhtnicityInome
  
  # Se elabora distribuci�n de frecuencia porcentual
  
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
  
  # C�lculo de Chi Cuadrado e �ndice de Cramer
  
  tablaEhtnicityInome = tablaEhtnicityInome[-4,-6]
  tablaEhtnicityInome
  
  options("digits"=4)               #defino un decimal 
  
  chi.cuad = chisq.test(tablaEhtnicityInome, simulate.p.value = TRUE)
  chi.cuad
  
  chi.cuad = chi.cuad$statistic
  chi.cuad
  
  cramer = sqrt(chi.cuad/(sum(tablaEhtnicityInome)*2))
  cramer
  
 
  # Se pide 7: Una comparaci�n de las distribuciones de las dos variables 
  # cuantitativas seleccionadas mediante un diagrama de dispersi�n
  
  
  plot(Credit$Income, Credit$Rating, xlab="Income", ylab="Rating", 
       main="Diagrama de dispersi�n") 
  
  
  # agrego la recta de regresion al diagrama de dispersion
  
  reg = lm(Rating~Income, Credit) 
  reg
  abline(reg) 
  
  # Calculo de la covarianza
  cov(Credit$Income, Credit$Rating)
  
  # Calculamos coeficiente de correlaci�n lineal de Pearson entre las variables
  cor(Credit$Income, Credit$Rating)
  
  # Comprobamos que hay correlaci�n positiva y por lo tanto las variables est�n relacionadas
  
  
  # BONUS
  
  # La variable cualitativa es Ethnicity
  # Se pide un an�lisis de la distribuci�n por grupos de cada variable cuantitativa 
  # por clases de la variable cualitativa.

  
  with(Credit,
       plot(Credit$Rating, Credit$Income,
            pch=as.numeric(Credit$Ethnicity), cex=1.2, ylab="Rating", xlab="Income"))
  
  legend( x= "bottomright",
          legend=c("African-American", "Caucasian", "Asian"), 
          pch=as.numeric(Credit$Ethnicity), cex=0.8)
  
