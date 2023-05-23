# Módulo 7: Machine Learning II: Modelos para la Clasificación y Segmentación
###### Trabajo final del Módulo 7 del Progama Experto en Ciencia de Datos
![](https://github.com/daperalt8/Mod7/blob/main/Experto%20en%20Ciencia%20de%20Datos.png)
------------
![](https://github.com/daperalt8/Mod7/blob/main/Secci%C3%B3n%20A.png)
------------
    library(foreign)
    library(dplyr)
    library(caret)
    library(ROCR)
    library(e1071)
    library(reshape2)
    library(pROC)
    library(ROSE)
    library(ggplot2)
    library(ROCR)
------------
    datos <- read.spss("C:\\Users\\Unemi\\Downloads\\ENV_2017.sav",
                       use.value.labels = F,
                       to.data.frame = T)
    datos$prov_nac <- as.numeric(as.character(datos$prov_nac))
    nuevadata <- datos %>% filter(prov_nac==13)%>%
      select(peso,
             talla,
             sem_gest,
             sexo,
             edad_mad,
             sabe_leer,
             con_pren)%>%
      filter(
        peso!=99,
        talla!=99,
        sem_gest!=99,
        con_pren!=99,
        sabe_leer!=9)%>%
      mutate(peso=if_else(peso>2500,1,0),
             sexo=if_else(sexo==1,0,1),
             sabe_leer=if_else(sabe_leer==1,1,0),
             con_pren=if_else(con_pren>=7,1,0),
             edad2=edad_mad^2)
    nuevadata$peso <- factor(nuevadata$peso)
    nuevadata <- nuevadata %>%
                 mutate(peso=recode_factor(
                   peso,
                   `0`="no.adecuado",
                   `1`="adecuado"))

------------
1. - **Se carga la base de datos de nacidos vivos, y se filtra información para la provincia de Manabí, también se elimina de la base de datos las observaciones que no tienen información, la variable peso se cambia a tipo factor debido a que es la variable de estudio, el peso del nacido vivo se codifica como "1" en el caso de que el peso sea mayor a 2500kg ya que, se considera como un peso adecuado del nacido vivo, y de las otras variables se realiza las transformaciones y categorías necesarias para que nuestro modelo pueda correr
#Base de Datos de Nacidos Vivos 
![](https://github.com/daperalt8/Mod7/blob/main/Base%20de%20datos%20sin%20Datawrangling.png)
------------
 ```r
 set.seed(1234)
    entrenamiento <- createDataPartition(nuevadata$peso,
                                         p=0.1,list=F)
    modelo.tuneado <- tune(svm,
                            peso ~.,
                            data=nuevadata[entrenamiento,],
                            ranges = list(cost=c(0.001,0.01,0.1,1,5,10,50)),
                            kernel="linear",
                            scale=T,
                            probability=TRUE)
    
    ggplot(data=modelo.tuneado$performances,
           aes(x=cost,y=error))+
      geom_line()+
      geom_point()+
      labs(title="Error de validacion vs hipeparametro C")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
```
------------

 ![](https://github.com/daperalt8/Mod7/blob/main/Imagen2.png)
 
------------
  - Se observa en el gráfico que la taza de error cae de forma drástica a medida que el costo va aumentando, sin embargo el proceso de cross-validation muestra que existe un costo que consigue un error muy bajo
 ------------
 ```r
mejor.modelo <- modelo.tuneado$best.model
summary(mejor.modelo)
```
------------
#Mejor modelo através del Cross-Validation
![](https://github.com/daperalt8/Mod7/blob/main/Mejor%20modelo.png)
------------
- El mejor modelo que minimiza el error sería el que tenga un costo de 0.1 con 457 vectores de soporte clasificado en dos clases "adecuado" y "no adecuado".
------------
```r
ajustados.mejor.modelo <- predict(mejor.modelo,
                                  nuevadata[entrenamiento,],
                                  type="prob",
                                  probability = T)

confusionMatrix(ajustados.mejor.modelo,
                nuevadata$peso[entrenamiento],
                positive = levels(nuevadata$peso)[2])

pred <- prediction(attr(ajustados.mejor.modelo,
                        "probabilities")[,2],
                   nuevadata$peso[entrenamiento])

perf <- performance(pred,"tpr","fpr")



plot(perf,colorize=T,lty=3)
abline(0,1,col="black")

aucmodelo1 <- performance(pred,measure = "auc")
aucmodelo1 <- aucmodelo1@y.values[[1]]
aucmodelo1
```
------------
#Matriz de Confusión del Mejor Modelo
![](https://github.com/daperalt8/Mod7/blob/main/Confusi%C3%B3n%20Matrix%20del%20mejor%20modelo.png)
------------
#Curva ROC del Mejor Modelo
![](https://github.com/daperalt8/Mod7/blob/main/Curva%20ROC%20del%20mejor%20modelo.png)
------------
- El mejor modelo tiene una precisión de clasificación muy bueno ya que su valor está muy cercano a 1, por otra parte clasifica con una probabilidad de 0.9243 el peso del nacido vivo cuando es el adecuado y con una probabilidad de 0.75 cuando el peso no es el adecuado, también la curva ROC es muy buena ya que la curva esta muy cerca de la parte superior sin embargo, la sensitividad es muy alta y la especificidad es muy baja lo que quizás se debe a un problema de desproporcionalidad muestral, lo que se va a corroborar más adelante o podría deberse aun problema del umbral de discriminación que es por defecto 0,5.
------------
    indice <- which.max(slot(max.accuracy,"y.values")[[1]])
    acc <- slot(max.accuracy,"y.values")[[1]][indice]
    cutoff <- slot(max.accuracy,"x.values")[[1]][indice]
    print(c(accuracy=acc,
          cutoff=cutoff))
##### Punto de Corte óptimo que Maximiza el Accuracy
![](https://github.com/daperalt8/Mod7/blob/main/Cutoff.png)
------------
- El modelo evaluado con el punto de corte de "0.924" tiene un valor menor del accuracy en comparación con el modelo evaluado del umbral de "0.5" por defecto y con el valor del cutoff, sin embargo la sensitividad y la sensitivad son buenos y clasifica los adecuados con una probabilidad de 0.9796, pero el valor de probabilidad de clasificación de los nacidos vivos con un peso adecuado es insignificante.
------------
        train_data <- nuevadata[entrenamiento, ]
    roses  <- ROSE(peso ~.,
                           data = train_data,seed = 1)$data
    modelo.rose <- tune(svm, peso ~ .,
                         data=roses,
                         ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50)),
                         kernel = "linear",
                         scale=T,
                         probability = TRUE)
    mejor.modelo.rose <- modelo.rose$best.model
    ajustadosrose <- predict(mejor.modelo.rose,
                              roses, type="prob",probability=TRUE)
    confusionMatrix(roses$peso,ajustadosrose,
                    dnn = c("Actuales", "Predichos"),
                    levels(ajustadosrose)[1])
 ------------
 ##### Matriz de Confusión con Remuestreo ROSES
![](https://github.com/daperalt8/Mod7/blob/main/Consusi%C3%B3n%20Matrix%20Remuestreo%20Roses.png)
------------
- La matriz de confusión tiene un accuracy casi del mismo valor que el modelo evaluado con 0.924, al igual que la sensitividad y especificidad, lo mejor del modelo con remuestreo ROSE es que mantiene la probabilidad para predecir si un nacido vivo nace con un peso "adecuado" y mejora la probabilidad para predecir si peso del nacido es no "adecuado", en resumen había un problema de desproporcionalidad muestral, ya que se podría decir que el modelo con remuestreo es el mejor de todos los anteriores modelados. 
------------

#### # ROC del mejor modelo vs ROC con remuestreo ROSES
![](https://github.com/daperalt8/Mod7/blob/main/ROC%20mejor%20%2Codelo%20vs%20ROC%20con%20remuestreo%20ROSES.png)
------------
- Según el gráfico de la parte superior parecería que no hay diferencia significativa en las dos curvas ROC, sin embargo cuando observamos la matriz de confusión de las dos curvas podemos darnos cuenta que hay diferencia significativa en los valores de la especifficidad y sensitividad, así tambien en las probabilidades para clasificar un "peso adecuado" y "no adecuado"




