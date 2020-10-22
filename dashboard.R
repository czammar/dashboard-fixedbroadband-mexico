# Install relevant packages if any is missing
list.of.packages <- c("shinny","plotly","ggplot2", "dplyr", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load data source of broadband in Mexico for municipalites
df <- read_csv("Fixed_Broadband_Mexico.csv")

# ggplot theme set up
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .95),
  panel.background = element_blank(),
  plot.background = element_blank()
)

# Vector of name of categorical variables
categorical_names <- df %>% select(which(sapply(.,is.character))) %>% colnames() 

######----- User interface design-----#####

ui<-fluidPage(
  # Application title
  titlePanel("Exploratory data analysys for Fixed Broadband in Mexico"),
  
  # Panel title
  headerPanel("Plot options"),
  
  #Left panel specifications
  sidebarPanel
  ( # Variables and type of plot to show
    selectInput("dataset","Data:", choices =list(mexico_fixedbroadband = "df"), selected=NULL), #
    # Field 1 to compare
    selectInput("variable1","Variable 1:", names(df)),
    # Field 2 to compare
    selectInput("variable2","Variable 2", names(df)), 
    
    selectInput("variable3","Variable 3 (for group dissagreggation)", choices = NULL), # Campo para seleccionar variable 3 a cruzar de manera que se desagreguen los plots con ella 
    selectInput("plot.type","Plot Type:", # Tipos de gráficas que permitirá definir el dashboard
                list(boxplot = "boxplot", 
                     histograma = "histogram", 
                     densidad = "density", 
                     barras = "bar", 
                     scatterplot = "scatterplot")
    ),
    checkboxInput("show.points", "Agregar puntos con Jitter", TRUE) # Campo para decidir si agregan a los boxplots los puntos con algun jitter
  ),
  
  # Llamaos a los graficos al panel del dashboard
  mainPanel(
    plotlyOutput("distPlot"), # Dasboard univariado
    plotlyOutput("p"), # Dasboard bivariado
    plotlyOutput("q") # Dashboard multivariado
  )
)


##### ----- Diseno del servidor para montar el dashboard ----- ######

server<-(function(input, output, session){
  
  #Actualizacion de las variables elegidas por el usuario en el panel lateral izquierdo
  observe({
    if(!exists(input$dataset)) return() # Revisamos que si se hallan cargado los datos de autos
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "variable1", choices = var.opts) # usuario variable 1 - de entre todas las variables de la base
    updateSelectInput(session, "variable2", choices = var.opts) # usuario variable 2 - de entre todas las variables de la base

    # usuario elige unicamente entre las variables categoricas para desglosar los graficos con face_wrap
    updateSelectInput(session, "variable3", choices = categorical_names
                      )
  })
  
  # Campo para mostar etiquetas del tipo de plot mostrado en funcion de grafica seleccionada en el panel
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density Plot",
           "bar" 		=	"Bar Grapj",
           "scatterplot" = "Scatterplot")
  })
  

  #get data object
  get_data<-reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    obj<-list(data=get(input$dataset),
              variable1=input$variable1,
              variable2=input$variable2,
              variable3=input$variable3
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$variable1,obj$variable2,obj$variable3) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()

    return(obj)
    
  })

  
######-----  Univariate plot -----######
  output$distPlot <- renderPlotly({
    plot.obj<-get_data()
    
    # Check we selectec all parameters
    if(is.null(plot.obj)) return()
    
    # Check of variables to compare
    if(plot.obj$variable1 == "") return()

    if(!plot.obj$variable1 %in% categorical_names){
    p<- ggplot(plot.obj$data, aes_string(plot.obj$variable1))+ geom_histogram(aes(y=..density..), colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") 
    p<-p+labs(
      x 		= input$variable1,
      title = "Univariate plot: variable 1"
      )
      p
      
    }
    else{
    p<- ggplot(plot.obj$data, aes_string(plot.obj$variable1, fill=plot.obj$variable1))+ geom_bar()
    p<- p + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    p<-p+labs(
      #   #fill 	= as.factor(input$variable1),
      x 		= input$variable1,
      title = "Univariate plot: variable 1"
      )
    p
    }
  })
  
######-----  Bivariate  -----######
  output$p <- renderPlotly({  
    plot.obj<-get_data()
    
    # revisamos que se hayan agregado los datos especificados para hacer 
    # las graficas, sino paramos
    if(is.null(plot.obj)) return()
    
    # verificamos que el usuario ha establecido las variables a cruzar
    if(plot.obj$variable1 == "" | plot.obj$variable2 =="") return()
    
    # Variable para indicarle a ggplot el tipo de grafico que ha selecionado el usuario
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.65,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge"),
                      "scatterplot" =	geom_point()
    )
    
    # Boxplots
    # Nota: si el usuario marca la casilla de añadir jitter se muestran los 
    # puntos con ruido para evitar overplotting
    if(input$plot.type=="boxplot")	{
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable2,
                  y 		= plot.obj$variable1,
                  fill 	= plot.obj$variable2 # llenamos grafica con variable 2
                )
      ) + plot.type
      
      if(input$show.points==TRUE) # add noise to boxplot plot for jittering
      {
        p<-p+ geom_point(color='black',alpha=0.35, position = 'jitter')
      }
      
    } else {
      # Scatterplots
      if(input$plot.type=="scatterplot")	{
        p<-ggplot(plot.obj$data,
                  aes_string(
                    x 		= plot.obj$variable2,
                    y 		= plot.obj$variable1,
                    fill 	= plot.obj$variable2
                  )
        ) + plot.type + geom_smooth(method = "lm") # agrega linea de tendencia con suavizado
        
      }else{
        # Sintaxis para generar el resto de plots
        p<-ggplot(plot.obj$data,
                  aes_string(
                    x 		= plot.obj$variable1,
                    fill 	= plot.obj$variable2,
                    group 	= plot.obj$variable2
                  )
        ) + plot.type
      }
    }
    
    # Se agregan descripciones de los ejes y codigo de colores
    p<-p+labs(
      fill 	= input$variable2,
      x 		= input$variable2,
      y 		= input$variable1,
      title = "Bivariado: variables 1 y 2"
    )  +
      .theme # agrega tema personalizado de ggplot
    print(p)
  })
    
######----- Multivariado ----- #####
    output$q <- renderPlotly({  
    plot.obj<-get_data()
    
    # revisamos que se hayan agregado los datos especificados para hacer las graficas, sino paramos
    if(is.null(plot.obj)) return()
    
    # verificamos que el usuario ha establecido las variables a cruzar
    if(plot.obj$variable1 == "" | plot.obj$variable2 ==""| plot.obj$variable3 =="") return()
    
    # Variable para indicarle a ggplot el tipo de grafico que ha selecionado el usuario
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.45,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge"),
                      "scatterplot" =	geom_point()
    )
    
    # Boxplots
    # Nota: si el usuario marca la casilla de añadir jitter se muestran los puntos con ruido para evitar overplotting
    if(input$plot.type=="boxplot")	{
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable2,
                  y 		= plot.obj$variable1,
                  fill 	= plot.obj$variable2 # Llenamos con la segunda variable
                )
      ) + plot.type + facet_wrap(~plot.obj$variable3) # se hace un face_wrat con la variable 3 seleccionada
      
      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')     # Nota: si el usuario marca la casilla de añadir jitter se muestran los puntos con ruido para evitar overplotting
        
      }
      
    } else {
      # Scatterplot
      if(input$plot.type=="scatterplot")	{
        p<-ggplot(plot.obj$data,
                  aes_string(
                    x 		= plot.obj$variable2,
                    y 		= plot.obj$variable1,
                    fill 	= plot.obj$variable2 # llenamos con la segunda variables
                  )
        ) + plot.type + geom_smooth(method = "lm") + facet_wrap( ~ get(plot.obj$variable3), ncol=3) # se hace un face_wrat con la variable 3 seleccionada
        
      }else{
        
        p<-ggplot(plot.obj$data,
                  aes_string(
                    x 		= plot.obj$variable1,
                    fill 	= plot.obj$variable2,
                    group 	= plot.obj$variable2
                    )
        ) + plot.type + facet_wrap( ~ get(plot.obj$variable3), ncol=3) # se hace un face_wrat con la variable 3 seleccionada
      }
    }
    
    # Se agregan descripciones de los ejes y codigo de colores
    p<-p+labs(
      fill 	= input$variable2,
      x 		= input$variable2,
      y 		= input$variable1,
      title = "Multivariado: variables 1 y 2, desagrupadas por variable 3"
    )  
    print(p)
  })
  
  
})


# Corremos la aplicacion con la interfaz de usuario y el servidor descrito previamente
shinyApp(ui = ui, server = server)
