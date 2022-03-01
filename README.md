# Analysis of Credit data obtained from the ISLR package

## 1 – Descripción de la base de datos

La base de datos utilizada en el siguiente informe fue obtenida del paquete ISLR y se llama
Credit. 
La misma contiene información de balances de crédito de las tarjetas de 400 clientes. 

El objetivo del set de datos es predecir que clientes no van a pagar su deuda.

La base de datos contiene 400 elementos y tiene 11 variables que son:

* `Income`: ingreso en decenas de miles de dólares.

* `Limit`: límite de crédito.

* `Rating`: puntaje de crédito.

* `Cards`: número de tarjetas de crédito.

* `Age`: edad en años.

* `Education`: número de años de educación.

* `Gender`: Masculino o Femenino.

* `Student`: Si o No dependiendo si la persona fue estudiante.

* `Married`: Si o No dependiendo si la persona estuvo casada.

* `Ethnicity`: indicador de la etnia de la persona (Afro-Americano, Caucásico o Asiático).

* `Balance`: Promedio del balance de la tarjeta de crédito en dólares

## 2 – Clasificación de las variables

Variables Cualitativas

Nominales 

Género
Estudiante
Casado
Etnia

Variables Cuantitativas

Razón

Ingresos
Límite
Tarjetas
Edad
Educación
Saldo
Rating

Todas las variables son discretas.

## 3 – Tipo de datos contenidos en la base

De corte transversal 

## 4 – Análisis Descriptivo

Variables cuantitativas elegidas: Rating e Income.

Variable cualitativa elegida: Ethnicity.

Los departamentos de riesgo de las empresas de crédito asignan un Rating que depende de
varios factores. En el siguiente trabajo se busca analizar si hay una correlación entre el ingreso
(Income), el Rating crediticio (Rating) y la etnia de la persona (Ethnicity). Considerando que
Estados Unidos tiene una larga tradición multicultural, el objetivo es entonces determinar si
hay prejuicios a la hora de asignar un determinado rating a un individuo.

### a - Análisis de la variable cualitativa
<br />
Tabla de Frecuencias:  
  
<br />
-------------------------------------------------------------------------
<br />

|                   |  `Frec_Abs`  |  `Rel_Credit`  |  `Por_Credit`  |  `Frec_Abs_Acum`  |  `Frec_Rel_Acum`  |  `Frec_Por_Acum`  |
| ------------------|:------------:| --------------:| --------------:| -----------------:| -----------------:| -----------------:|
| `African American`|       99     |    0.2475      |    24.75       |        99         |      0.2475       |      24.75        |
| `Asian`           |      102     |    0.2550      |    25.50       |       201         |      0.5025       |      50.25        |
| `Caucasian`       |      199     |    0.4975      |    49.75       |       400         |      1.0000       |      100.00       |







