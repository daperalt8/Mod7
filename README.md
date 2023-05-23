Se carga la base de datos de nacidos vivos, y se filtra información para la provincia de Manabí, también se elimina de la base de datos las observaciones que no tienen información, la variable peso se cambia a tipo facor debido a que es la variable de estudio, el peso del nacido vivo se codifica como “1” en el caso de que el peso sea mayor a 2500kg ya que que se considera como un peso adecuado del nacido vivo, y de las otras variables se realiza las transformaciones y categorías necesarias para que nuestro modelo pueda correr.

```r
```r
set.seed(1234)

    ```r
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
```
![https://github.com/

