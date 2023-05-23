# Módulo 7: Machine Learning II: Modelos para la Clasificación y Segmentación
###### Trabajo final del Módulo 7 del Programa Experto en Ciencia de Datos
![](https://github.com/daperalt8/Mod7/blob/main/Experto%20en%20Ciencia%20de%20Datos.png)
------------
![](https://github.com/daperalt8/Mod7/blob/main/Secci%C3%B3n%20A.png)
# Librerías
------------
    biblioteca (extranjera)
    biblioteca (dplyr)
    biblioteca
    biblioteca (ROCR)
    biblioteca (e1071)
    biblioteca (remodelar2)
    biblioteca (pROC)
    biblioteca (rosa)
    biblioteca (ggplot2)
    biblioteca (ROCR)
------------
# Exportando Base de Datos desde SPSS
    datos <- read.spss("C:\\Usuarios\\Unemi\\Descargas\\ENV_2017.sav",
                       usar.valor.etiquetas = F,
                       a.data.frame = T)  
    datos$prov_nac <- as.numeric(as.character(datos$prov_nac))
    nuevadata <- datos %>% filter(prov_nac==13)%>%
      seleccionar(peso,
             talla,
             sem_gesto,
             sexo,
             edad_mad,
             sabe leer,
             con_pren)%>%
      filtrar(
        pesos!=99,
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
    datos nuevos <- datos nuevos %>%
                 mutate(peso=recode_factor(
                   peso,
                   `0`="no.adecuado",
                   `1`="adecuado"))
------------
1. - **Se carga la base de datos de nacidos vivos, y se filtra información para la provincia de Manabí, también se elimina de la base de datos las observaciones que no tienen información, la variable peso se cambia a tipo factor debido a que es la variable de estudio, el peso del nacido vivo se codifica como "1" en el caso de que el peso sea mayor a 2500kg ya que, se considera como un peso adecuado del nacido vivo, y de las otras variables se realiza las transformaciones y categorias necesarias para que nuestro modelo pueda correr
------------
# Base de Datos de Nacidos Vivos
![](https://github.com/daperalt8/Mod7/blob/main/Base%20de%20datos%20sin%20Datawrangling.png)
------------
```r
set.seed(1234)
    entrenamiento <- createDataPartition(nuevosdatos$peso,
                                         p=0.1,lista=F)
    modelo.tuneado <- tune(svm,
                            peso~.,
                            datos=nuevosdatos[entrenamiento,],
                            rangos = lista(costo=c(0.001,0.01,0.1,1,5,10,50)),
                            kernel="lineal",
                            escala=T,
                            probabilidad=VERDADERO)
    
    ggplot(data=modelo.tuneado$rendimientos,
           aes(x=coste,y=error))+
      geom_line()+
      geom_punto()+
      labs(title="Error de validacion vs hipeparámetro C")+
      tema_bw()+
      tema(trama.título = elemento_texto(hjust = 0.5))
```
------------
![](https://github.com/daperalt8/Mod7/blob/main/Imagen2.png)
 
------------
  - Se observa en el gráfico que la taza de error cae de forma drástica a medida que el costo va aumentando, sin embargo el proceso de validación cruzada muestra que existe un costo que obtiene un error muy bajo
------------
```r
mejor.modelo <- modelo.tuneado$best.modelo
resumen(mejor.modelo)
```
------------
# Mejor Modelo a través del Cross Validation
![](https://github.com/daperalt8/Mod7/blob/main/Mejor%20modelo.png)
------------
- El mejor modelo que minimice el error sería el que tenga un costo de 0.1 con 457 vectores de soporte clasificado en dos clases "adecuado" y "no adecuado".
------------
```r
ajustados.mejor.modelo <- predict(mejor.modelo,
                                  nuevadata[entrenamiento,],
                                  tipo = "probable",
                                  probabilidad = T)
confusionMatrix(ajustados.mejor.modelo,
                nuevadata$peso[entrenamiento],
                positivo = niveles(nuevosdatos$peso)[2])
pred <- predicción(attr(ajustados.mejor.modelo,
                        "probabilidades")[,2],
                   nuevadata$peso[entrenamiento])
rendimiento <- rendimiento(pred,"tpr","fpr")
trazar (rendimiento, colorear = T, lty = 3)
abline(0,1,col="negro")
aucmodelo1 <- rendimiento(pred,medida = "auc")
aucmodelo1 <- aucmodelo1@y.valores[[1]]
aucmodelo1
```
------------
# Matriz de Confusión del Mejor Modelo
![](https://github.com/daperalt8/Mod7/blob/main/Confusi%C3%B3n%20Matrix%20del%20mejor%20modelo.png)
------------
# Curva ROC del Mejor Modelo
![](https://github.com/daperalt8/Mod7/blob/main/Curva%20ROC%20del%20mejor%20modelo.png)
------------
- El mejor modelo tiene una precisión de clasificación muy bueno ya que su valor está muy cerca a 1, por otra parte clasifica con una probabilidad de 0.9243 el peso del nacido vivo cuando es el adecuado y con una probabilidad de 0.75 cuando el peso no es el adecuado, también la curva ROC es muy buena ya que la curva está muy cerca de la parte superior sin embargo, la sensibilidad es muy alta y la especificidad es muy baja lo que quizás se debe a un problema de desproporcionalidad muestral, lo que se va a corroborar más adelante o podría deberse aun problema del umbral de discriminación que es por defecto 0,5.
------------
    índice <- cuál.max(slot(max.accuracy,"y.values")[[1]])
    acc <- slot(máx.precisión,"y.valores")[[1]][índice]
    corte <- ranura(máx.precisión,"x.valores")[[1]][índice]
    imprimir (c (precisión = acc,
          corte=corte))
------------
# Punto de Corte Óptimo que Maximiza el Accuracy
![](https://github.com/daperalt8/Mod7/blob/main/Cutoff.png)
------------
- Según los resultados de la parte superior el umbral óptimo sería de 0.568994, lo que se espera es que el umbral óptimo mejore las predicciones, la especificidad, la sensitividada, lo que permitiría clasificar de una mejor forma cuando el peso de un nacido vivo en la provincia de Manabí es el adecuado y cuando no.
------------
# Evaluación del Modelo con el Punto de Corte Óptimo
        umbral <- as.numeric(cutoff)
        prediccionescutoff <- attr(ajustados.mejor.modelo,
                               "probabilities")[,1]
    prediccionescutoff <- as.numeric(prediccionescutoff) 
    predCut <- factor(ifelse(prediccionescutoff>umbral,1,0))   
    matrizpuntocorte <- data.frame(real=nuevadata$peso[entrenamiento],
                                   predicho=predCut)
        matrizpuntocorte <- matrizpuntocorte %>% mutate(predicho=recode_factor(predicho,
                                                                            `0`="no.adecuado",
                                                                            `1`="adecuado"))   
    confusionMatrix(matrizpuntocorte$predicho,
                    matrizpuntocorte$real,
                    positive = "adecuado") 
    curvaroc <- plot.roc(nuevadata$peso[entrenamiento],
                         as.vector(prediccionescutoff),
                         precent=TRUE,
                         ci=TRUE,
                         print.auc=TRUE,
                         threholds="best",
                         print.thres="best")
    abline(v=cutoff)
------------
# Matriz de Clasificación del Mejor Modelo con el Cutoff
![](https://github.com/daperalt8/Mod7/blob/main/Clasificaci%C3%B3n%20del%20mejor%20modelo%20con%20Cutoff.png)
------------
# Curva ROC del Mejor Modelo con el Cutoff
![](https://github.com/daperalt8/Mod7/blob/main/Curva%20ROC%20con%20el%20punto%20de%20corte%20optimo.png)
------------
- El modelo con el punto de corte óptimo no ha mejorado significativamente en la precisión, ni en la especificidad y tampoco tiene mejoras en la sensitividad, de igual manera la curva ROC que en el punto de corte óptimo mi modelo tiene una sensitivad de aproximadamente 0.84, sin embargo la especificidad no ha mejorado en gran manera, se sospecha que hay una desproporcionalidad muestral para clasificar los "1" y los "0". 
------------
# Pronóstico fuera de la muestra con punto de corte por defecto (0.5) y con punto de corte de 0.924

    
    newdata2 <- data.frame(talla=45,
                           sem_gest=38,
                           sexo=1,
                           edad_mad=30,
                           sabe_leer=1,
                           con_pren=1,
                           edad2=900)
    
    pronostico1 <- predict(mejor.modelo, newdata2,probability = TRUE)
    pronostico1
    
    
    pronostico2 <- ifelse(attr(pronostico1,"probabilities")[1]>0.924,1,0)
    pronostico2
------------
# Pronóstico sin punto de corte y con punto de corte óptimo
![](https://github.com/daperalt8/Mod7/blob/main/Pron%C3%B3stico%20sin%20punto%20de%20corte%20y%20con%20punto%20corte.png)
------------                        
- La clasificación para el modelo con un corte umbral de 0.5 por defecto clasifica según el valor de las variables de "newdata2" como un niño que nacerá con un peso adecuado sin embargo con un umbral de 0.924 el niño se clasifica como "0", es decir un niño que nacera con un peso no adecuado.A continuación se va a evaluar el modelo con el valor de umbral que está mas distante a la línea de 45.
------------
# Evaluando el modelo con un corte de umbral 0.924

    umbral1 <- 0.924
    
    pred.umbral1 <- attr(ajustados.mejor.modelo,
                               "probabilities")[,1]
    
    pred.umbral1 <- as.numeric(pred.umbral1)
    
    
    pred.cut.umbral1 <- factor(ifelse(pred.umbral1>umbral1,1,0))
    
    
    matriz.corte <- data.frame(real=nuevadata$peso[entrenamiento],
                                   predicho=pred.cut.umbral1)
    
    matriz.corte <- matriz.corte %>% mutate(predicho=recode_factor(predicho,                                                                    `0`="no.adecuado",
                                                        `1`="adecuado"))
    
    
    
    confusionMatrix(matriz.corte$predicho,
                    matriz.corte$real,
                    positive = "adecuado")
------------
# Matriz de Clasificación con Cutoff de 0.924
![](https://github.com/daperalt8/Mod7/blob/main/MC%20con%20cutoff%200.924.png)
------------
- El modelo evaluado con el punto de corte de "0.924" tiene un valor menor del precision en comparación con el modelo evaluado del umbral de "0.5" por defecto y con el valor del cutoff, sin embargo la sensibilidad y la sensitivad son buenos y clasifica los adecuados con una probabilidad de 0.9796, pero el valor de probabilidad de clasificación de los nacidos vivos con un peso no adecuado es insignificante.
------------
# Remuestreo ROSES
        tren_datos <- nuevosdatos[entrenamiento, ]
    rosas <- ROSA(peso ~.,
                           datos = tren_datos,semilla = 1)$datos
    modelo.rose <- tune(svm,peso~.,
                         datos=rosas,
                         rangos = lista(costo = c(0.001, 0.01, 0.1, 1, 5, 10, 50)),
                         núcleo = "lineal",
                         escala=T,
                         probabilidad = VERDADERO)
    mejor.modelo.rose <- modelo.rose$best.model
    ajustadosrose <- predict(mejor.modelo.rose,
                              rosas, escriba="probabilidad",probabilidad=VERDADERO)
    confusionMatrix(rosas$peso,ajustadosrose,
                    dnn = c("Actuales", "Predichos"),
                    niveles(rosa ajustados)[1])
------------
# Matriz de confusión con Remuestreo ROSES
![](https://github.com/daperalt8/Mod7/blob/main/Consusi%C3%B3n%20Matrix%20Remuestreo%20Roses.png)
------------
- La matriz de confusión tiene una precisión casi del mismo valor que el modelo evaluado con 0.924, al igual que la sensibilidad y especificidad, lo mejor del modelo con remuestreo ROSE es que mantiene la probabilidad para predecir si un nacido vivo nace con un peso " adecuado" y mejora la probabilidad para predecir si el peso del nacido no es "adecuado", en resumen había un problema de desproporcionalidad muestral, ya que se podría decir que el modelo con remuestreo es el mejor de todos los modelados anteriores.
------------
# ROC del Mejor Modelo vs ROC con Remuestreo ROSES
![](https://github.com/daperalt8/Mod7/blob/main/ROC%20mejor%20%2Codelo%20vs%20ROC%20con%20remuestreo%20ROSES.png)
------------
- Según el gráfico de la parte superior parecería que no hay diferencia significativa en las dos curvas ROC, sin embargo cuando observamos la matriz de confusión de las dos curvas podemos darnos cuenta que hay diferencia significativa en los valores de la especifficidad y sensibilidad, así también en las probabilidades para clasificar un "peso adecuado" y "no adecuado"
------------
# Pronóstico Tuneado vs Pronóstico con Remuestreo ROSES

    
    pred1 <- prediction(attr(ajustadosrose,
                            "probabilities")[,2],
                       nuevadata$peso[entrenamiento])
    
    max.accuracy1 <- performance(pred1,measure = "acc")
    indice <- which.max(slot(max.accuracy1,"y.values")[[1]])
    acc <- slot(max.accuracy1,"y.values")[[1]][indice]
    cutoff1 <- slot(max.accuracy1,"x.values")[[1]][indice]
    
    print(c(accuracy=acc,
          cutoff1=cutoff))
    
    puntocorte <- as.numeric(cutoff1)
    
    prediccionescut <- attr(ajustadosrose,
                               "probabilities")[,1]
    
    prediccionescut <- as.numeric(prediccionescut)
    
    
    predCutt <- factor(ifelse(prediccionescut>puntocorte,1,0))
    
    
    matriz <- data.frame(real=nuevadata$peso[entrenamiento],
                                   predicho=predCutt)
    
    matriz <- matriz %>% mutate(predicho=recode_factor(predicho,
                                                                            `0`="no.adecuado",
                                                                            `1`="adecuado"))
    
    
    confusionMatrix(matriz$predicho,
                    matriz$real,
                    positive = "adecuado")
    
    
    
    curvaroc1 <- plot.roc(nuevadata$peso[entrenamiento],
                         as.vector(prediccionescut),
                         precent=TRUE,
                         ci=TRUE,
                         print.auc=TRUE,
                         threholds="best",
                         print.thres="best")
    
    newdata3 <- data.frame(talla=45,
                           sem_gest=38,
                           sexo=1,
                           edad_mad=30,
                           sabe_leer=1,
                           con_pren=1,
                           edad2=900)
    
    pronostico3 <- predict(mejor.modelo, newdata3,probability = TRUE)
    pronostico3
    pronostico4 <- predict(mejor.modelo.rose, newdata3,probability = TRUE)
    pronostico4
    ------------  
# Pronóstico
![](https://github.com/daperalt8/Mod7/blob/main/Pron%C3%B3stico.png)
------------
- Para los datos ingresados en las variables, el pronóstico con el mejor modelo sin muestreo lo clasifica como un peso "adecuado", pero el pronóstico del modelo con remuestreo lo clasifica como un peso "no adecuado", esto se debe a que la clasificación para pesos no adecuado era mala para el modelo tuneado, sin embargo esa probabilidad para clasificación mejoró con el remuestreo.
Esto quiere decir que mi modelo no se encontaba clasificando de buena manera el peso de un nacido vivo que es menor a los 2500 kg.
------------
#Comparación de Predicciones
    
    predicciones <- attr(ajustados.mejor.modelo,
                               "probabilities")[,1]
    predicciones <- as.numeric(predicciones)
    predd <- factor(ifelse(predicciones>0.924,1,0))
    
    predictt <- attr(ajustadosrose,
                               "probabilities")[,1]
    predictt <- as.numeric(predictt)
    preddi <- factor(ifelse(predictt>0.824,1,0))
    
    matriz.comparacion <- data.frame(pronóstico.mejor.modelo.punto.corte=predd,
                                   pronostico.remuestreo.punto.corte=preddi)
    head(matriz.comparacion,30)    
------------



    
    
    
    
    
    
