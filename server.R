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


cats <- sapply(dataset, is.character)
categoricas <- names(dataset)[cats] #Nos guardamos las variables categoricas

dataset$Watch.time.hours = dataset$Watch.time.Minutes./60
dataset$Stream.time.hours = dataset$Stream.time.minutes./60

dataset <- dataset[,-c(2,3)]

nums <- sapply(dataset, is.numeric)
continuas <- names(dataset)[nums] #Nos guardamos las variables numéricas



shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    
    #Nos quedamos con las continuas y el factor
    Top <- dataset %>% select(c(Channel,all_of(continuas),input$color)) 
    
    #Cogemos la variable definida en el ui en el subset de Top
    Variable=Top[,input$Variable]
    
    #Ordenamos en funcion a la variable
    Obs1 = Top %>%
                 arrange(desc(Variable)) %>% mutate(Channel = fct_reorder(Channel, Variable))
    
    #Cogemos las primeras observaciones definidas por el usuario
    Obs = head(Obs1,n=input$obs)
    
    #definimos la variable en el subset recortado
    Variable=Obs[,input$Variable]
    Color =Obs[,input$color]

    p <- Obs %>% ggplot(aes(x=reorder(Channel,Variable),y=Variable, fill=Color )) + 
      geom_col() 
    
    p <- p +
      scale_fill_viridis_d(direction = 1,option = "D")+
      theme(legend.position="right")+
      coord_flip() +
      labs(x = "Canal", y = input$Variable) +
      theme (axis.text.x = element_text(size=rel(1.5)),
             axis.text.y = element_text(size=rel(1.5)))
    
    title <- paste("Top",
                   "\t",
                   nrow(Obs),
                   "Streamers de Twitch")
    
    print(p + ggtitle(title))
    
  })
  
  output$plot2 <- renderPlot({

    if (input$log)
      
    logdataset = log(Filter(is.numeric, dataset))
    
    else
      
    logdataset = dataset
    
    
    Variable2=logdataset[,input$Variable2]
    
   p <- ggplot(logdataset, aes(x = Variable2, y = Followers.gained)) + 
      geom_point() +
      labs(x = input$Variable2, y = "Followers.gained")
   
      if (input$smooth)
        p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)

      if (input$lm)
        p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T,col="red")

    print(p)
  })
  
})