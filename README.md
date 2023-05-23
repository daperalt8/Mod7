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
 ![](https://github.com/daperalt8/Mod7/blob/main/Imagen2.png)
 
 - Se observa en el gráfico que la taza de error cae de forma drástica a medida que el costo va aumentando, sin embargo el proceso de cross-validation muestra que existe un costo que consigue un error muy bajo.
 ```r
mejor.modelo <- modelo.tuneado$best.model
summary(mejor.modelo)
```
 



