#
# Módulo visualización avanzada
#
# Ejercicio de fin de módulo
#
# Datos extraídos de:
#https://www.kaggle.com/aayushmishra1512/twitchdata/version/3/twitchdata-update.csv

#The mature means 18+ content or stream for adults as they might contain some explicit words or graphics. 
#And followers gained is the amount of followers they gained during the the year.
#Partner de twitch

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(colorspace)

# Cargamos los datos desde el repositorio github del módulo. Aqui y en el server.
#setwd("C:/Users/cente/Desktop/Master/Visualizacion Avanzada/Tarea/data/")
#dataset <- read.csv("C:/Users/cente/Desktop/Master/Visualizacion Avanzada/Tarea/data/twitchdata-update.csv")
dataset <- get(load(url("https://github.com/Alejandrocentellas/visualizacion/blob/main/twitchdata-update.rda?raw=true")))

summary(dataset)
str(dataset)

dataset[,(9:11)] <- lapply(dataset[,(9:11)], factor)

cats <- sapply(dataset, is.factor)
categoricas <- names(dataset)[cats] #Nos guardamos las variables categoricas

dataset$Watch.time.hours = dataset$Watch.time.Minutes./60
dataset$Stream.time.hours = dataset$Stream.time.minutes./60

dataset <- dataset[,-c(2,3)]

nums <- sapply(dataset, is.numeric)
continuas <- names(dataset)[nums] #Nos guardamos las variables numéricas

Top <- dataset %>% select(c(Channel,all_of(continuas),Language)) %>%
  arrange(desc(Followers)) %>% mutate(Channel = fct_reorder(Channel, Followers)) 

shinyUI(
  navbarPage("Shiny Visualización Avanzada", #navbarPage para meter pestañas
             tabPanel("Descripción del trabajo",
                      mainPanel( #Pagina principal
                        h1("Ejercicio Visualización Avanzada", align = "center"),
                        h2("Propuesto por Alejandro Centellas", align = "center"),
                        p("Mi propuesta para el ejercicio final del 
                          módulo de visualización avanzada consiste en ofrecer distintas
                          visualizaciones para un conjunto de datos que contienen a los streamers
                          más relevantes de Twitch en la actualidad."),
                        p("En esta app organizada en torno a pestañas con la función 
                                navbarPage iremos explorando y sacando conclusiones sobre nuestros datos gracias a la visualización. La aplicación
                          ha sido subida a shinyapps.io a la esta url: https://alexcentellas.shinyapps.io/Tarea/"),
                        p("Los datos han sido obtenidos a través de kaggle subidos por el usuario 
                          Aayush Mishra y actualizados por última vez el día 24-08-2020. 
                          No contienen valores perdidos y solo se ha pasado 
                          los datos tipo carácter a factor y se han transformado dos variables que venían en minutos.
                          A continuación haremos una breve descripción de las variables del dataset:"),
                        p("Channel: Canal único por cada streamer."),
                        p("Peak.Viewers: Número máximo de espectadores en el stream en el año pasado."),
                        p("Average.Viewers: Número medio de espectadores por stream."),
                        p("Followers: Número de seguidores del streamer"),
                        p("Followers.gained: Número de seguidores ganados en el año por el streamer"),
                        p("Views.gained: Número de espectadores ganados en el año por el streamer"),
                        p("Partnered: Indica si el streamer está dentro del programa de socios de twitch o no"),
                        p("Mature: Indica si el contenido del streamer es para mayores de 18 o pueda contener palabras explícitas"),
                        p("Language: Idioma del contenido del canal del streamer"),
                        p("Watch.time.hours: Horas de visualización acumuladas. Se ha transformado esta variable porque venía en minutos"),
                        p("Stream.time.hours: Horas de que ha realizado el streamer acumuladas. Se ha transformado esta variable porque venía en minutos"),
                        h2("Análisis sobre los streamers más importantes", align = "center"),
                        p("En la siguiente pestaña se representa un gráfico de barras. En el eje vertical se representan los canales de Twitch
                          de los distintos streamers. El usuario puede seleccionar el número de canales para la visualización hasta 50. En el eje horizontal
                          el usuario puede seleccionar la variable numérica que desea visualizar. Además de todo esto, el usuario
                          también puede seleccionar por qué factor visualizar los datos. Estos factores son el idioma, si el streamer es socio de Twitch
                          o si el contenido del streamer es para adultos. Para representar estos factores sobre cada streamer
                          se ha utilizado una paleta de colores y a la derecha del gráfico se puede consultar qué color corresponde con cada idioma."),
                        p("En todas las categorías destacan los streamers de habla inglesa porque es el grupo mayoritario en todas las variables
                          que se representan."),
                        p("Una de las variables que definirían la importancia de los streamers sería la de seguidores. Cuantos más seguidores,
                          más alcance tendrá el streamer en cuestión y más ingresos en consecuencia."),
                        p("Los streamers de habla inglesa son los que más seguidores tienen en general. Sin embargo,
                        los streamers españoles se están abriendo hueco en este mercado porque los tres que más seguidores 
                          obtuvieron en el 2019 son españoles: Auronplay, Rubius y TheGrefg."),
                        p("Por otra parte, podemos deducir que las horas que realiza un streamer no repercute en la cantidad de seguidores que
                          puede llegar a obtener porque si nos fijamos en los 20 streamers top de seguidores, ninguno de ellos aparece en el 
                          top 20 de streamers que más horas realizan. Además, hemos calculado la correlación de Pearson entre la variable
                          Followers y Stream.time.hours y es de -0.09. Es decir, prácticamente nula."),
                        p("También es relevante la cantidad de espectadores que un streamer tiene de media. Cuantos más tenga quiere decir
                          que tiene un público fiel y cada vez que abra directo en Twitch tendrá más personas viendo su stream y es más probable
                          que le hagan más donaciones y el streamer genere más ingresos."),
                        p("En esta categoría destacan dos canales con más de 100.000 espectadores de media que retransmiten un videojuego multijugador 
                           denominado Dota. Además, en el top 20 también aparecen streamers con muchos seguidores como Rubius o Auronplay."),
                        p("En cuanto al factor socio de Twitch, casi todos los streamers más importantes son parte del programa de socios de Twitch.
                          Esto es lógico porque para formar parte de ese programa se necesita un mínimo de afiliados y con esto el trabajo del streamer
                          se profesionalizaría. Por lo tanto, podemos decir que para ser un streamer importante y generar ingresos con Twitch es
                          prácticamente necesario ser parte del programa de socios."),
                        p("Si hablamos del contenido para adultos, en Twitch predomina un contenido apto para todos los públicos porque sobre todo
                          los que más consumen contenido de esta plataforma son jóvenes y adolescentes. Por lo tanto, si nos fijamos en el gráfico de
                          streamers más importantes vemos pocos canales con contenido para adultos. Donde más podemos apreciar canales 
                          con contenido para mayores de 18 es para la variable que mide la cantidad de horas de visualización de los canales. Para esta
                          variable hay más canales en el top 20 que para las demás pero estas visualizaciones no se traducen en más seguidores en Twitch."),
                        h2("Estudio de los seguidores"),
                        p("Por otra parte, para un streamer sería interesante estudiar cuántos seguidores puede llegar a ganar en el siguiente año. Para
                          ello en la última pestaña realizamos un gráfico de puntos en el que situamos como nuestra variable dependiente la variable
                          Followers.gained."),
                        p("En este gráfico, el usuario puede seleccionar la variable en el eje x para comprobar la relación que guarda con nuestra variable
                          objetivo. Además, puede seleccionar para que se represente gráficamente una regresión lineal o un suavizado LOESS."),
                        p("Al visualizar este gráfico nos damos cuenta de que nuestros datos son heterocedásticos. 
                          Esto es debido a que nuestras variables están sesgadas a la derecha, por lo tanto se distribuyen de forma asimétrica. Por poner
                          un ejemplo, en cuanto a la variable de seguidores. Habrá pocos streamers con una gran cantidad de seguidores, que son los que
                          hemos analizado en la segunda pestaña, sin embargo la mayoría de streamers no tendrán tantos por lo que es difícil comparar
                          unos con otros. Para estos casos una buena solución suele ser transformar nuestros datos a una escala logarítmica. Al hacer
                          esto estamos expandiendo los valores más pequeños y contrayendo los más altos, provocando así que nuestras variables se asemejen
                          más a una distribución normal. Si el usuario pincha en el checkbox que indica Logaritmo podrá comprobar que la visualización
                          mejora mucho debido a que al aplicar el logaritmo se aprecia una mayor homocedasticidad en los datos."),
                        p("Dicho esto, pasamos a analizar las relaciones entre la variable de Followers.gained con las demás variables numéricas de 
                          nuestro dataset, estas relaciones las analizaremos con el gráfico en escala logarítmica. Para empezar, parece que la variable
                          que más relación guarda con el número de seguidores conseguido es la del número de seguidores que ya posee el propio streamer.
                          Esto tiene sentido porque la cantidad de seguidores que tiene un streamer tiene una gran dependencia con los seguidores que 
                          consiguió el año pasado. Además, podemos observar que hay una relación lineal positiva para el logaritmo de estas variables."),
                        p("También apreciamos una relación lineal positiva con el número máximo de espectadores que ha tenido el streamer. Es muy
                          importante para los streamers hacerse viral y tener muchos espectadores porque cuanto mayor sea este pico, más cantidad de 
                          seguidores podrán conseguir. Sin embargo, no solo basta con hacerse viral una vez, si no que es igual de importante
                          tener una media de espectadores elevada para conseguir un mayor número de seguidores. De hecho si en una regresión 
                          múltiple incluimos las variables Peak.viewers y Average.viewers solamente es significativa la de Average.viewers. Probablemente
                          porque lo que explique esa última está muy relacionado con la variable Peak.viewers."),
                        p("La siguiente variable en orden de importancia para explicar nuestra variable objetivo sería las horas de visualización 
                          acumuladas. Podemos apreciar una relación positiva, por lo que a mayor número de horas de visualización que acumule el streamer
                          más seguidores acabará ganando. Sin embargo, al aplicar la recta de regresión lineal como el suavizado LOESS apreciamos que
                          los límites de sus intervalos de confianza se ensanchan a mayor valor de la x. Esto se debe a los valores atípicos de nuestro
                          dataset por lo que debemos tener en cuenta que los streamers más grandes pueden estar sesgando nuestra visualización."),
                        p("Por último las variables Views.gained y Stream.time.hours son las que menos relación guardan con la cantidad de seguidores
                          que ha ganado un streamer. Las nubes de puntos con estas variables indican que no existe mucha relación. Si nos fijamos en la
                          variable de las horas que realiza un streamer, en logaritmo podría parecer que incluso hay una relación lineal negativa
                          o no lineal entre nuestras dos variables. Podemos decir que aunque un streamer haga más horas de stream, esto no le 
                          garantiza que vaya a ganar más seguidores. De todas formas, si nos fijamos en esta variable sin el logaritmo veremos
                          tres puntos atípicos que son influyentes a la hora de pintar la regresión porque han conseguido una cantidad enorme de
                          seguidores haciendo menos horas de stream que la media. Justamente estos tres puntos son los streamers españoles que
                          hemos comentado previamente y aunque no hayan hecho tantas horas de stream es normal que hayan ganado tantos seguidores
                          porque llevan muchos años siendo conocidos en Youtube.")
                      )),
             tabPanel("Barplot",
                      fluidPage(
                          titlePanel("Streamers más importantes de Twitch"),
                                 
                            sidebarPanel(
                              selectInput('Variable',
                                      'Elige variable para eje X',
                                      continuas, 
                                      continuas[[3]]),
                          
                            numericInput(inputId = "obs",
                                     "Observations:",
                                     1, 
                                     min = 1,
                                     max = 50,
                                     value = 20),
                          
                            selectInput('color',
                                      'Elige factor para el gráfico',
                                      c('Language','Partnered','Mature'), 
                                      ),
                          
                            ),
                        
                            mainPanel(plotOutput(outputId = 'plot',
                                     height = 800,
                                     width = 1000))
                        
                        )
                        
                      ),
             tabPanel("Scatterplot",
                      fluidPage(
                        titlePanel("Relación entre seguidores ganados y otras variables"),
                        
                        sidebarPanel(
                          selectInput('Variable2',
                                      'Elige variable para eje X',
                                      continuas, 
                                      continuas[[3]]),
                          checkboxInput('lm', 'Regresión lineal'),
                          checkboxInput('smooth', 'Suavizado LOESS'),
                          checkboxInput('log', 'Logaritmo'),
                          
                        ),
                        
                        mainPanel(plotOutput(outputId = 'plot2',
                                             height = 800,
                                             width = 1000))
                        )
             )
             
          )
  )