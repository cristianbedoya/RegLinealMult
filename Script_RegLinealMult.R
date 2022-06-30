#Archivos requeridos para el laboratorio: 
#TrainDataDYSJTPFA.csv
#TestDataDYSJTPFA.csv

# Regresion Lineal Multiple

#Ver el directorio de trabajo actual (working directory)
direccion = getwd()

#Cambiar el directorio de trabajo
setwd(direccion)


# Leer los datos de entrenamiento del modelo (datos de crimenes y empleo para los años 2012, 2013, 2014)
TrainData_Crimes = read.csv("TrainDataDYSJTPFA.csv", header = TRUE, sep = ",")

# Revisar la correlacion entre pares de variables
correlacion = cor(TrainData_Crimes)

# b)
# Encontrar una funcion lineal que describa el numero de crimenes en funcion de todas las otras variables
Modelo_RegresionLinealMult = lm(Crimecount ~ Employment_Rate, data=TrainData_Crimes)

# Resumen del modelo de regresion lineal multiple
summary(Modelo_RegresionLinealMult)

# Coeficientes del modelo de regresion lineal multiple
coeff= coef(Modelo_RegresionLinealMult)
coeff

# Hacer pruebas
# Leer los datos de prueba (datos de crimenes y empleo para el año 2015)
TestData_Crimes = read.csv("TestDataDYSJTPFA.csv", header = TRUE, sep = ",")


# Predecir el numero de crimenes para el año 2015 basado en el modelo, usando los datos de prueba

predicciones_multiple_2015<-predict(Modelo_RegresionLinealMult, TestData_Crimes)

# Transformar el vector de prediccion en un data frame
df_predicciones_multiple<-data.frame(predicciones_multiple_2015)

# Agregar la columna de los crimenes reales al data frame de las predicciones 
df_predicciones_multiple$real_2015<-(TestData_Crimes$Crimecount)

# Graficar predicciones vs real
plot(df_predicciones_multiple$real_2015, df_predicciones_multiple$predicciones_multiple_2015, xlab="Real", ylab="Prediccion", col="red")+ abline(1,1, col="green")

# Evaluar performance - Calcular el error usando funciones de la libreria hydroGOF
library (zoo)
library (hydroGOF)

p <- predicciones_multiple_2015         # Predicciones numero de crimenes 2015 por mes
a <- TestData_Crimes$Crimecount         # Numero de crimenes reales

mae_regresion_mult <- mae (p, a)
rmse_regresion_mult <- rmse (p, a)





