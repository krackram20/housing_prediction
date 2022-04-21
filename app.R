
library(shinydashboard)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(randomForest)

options(scipen = 999)
moda <-  readRDS('rf.rds')


mapa <- read.csv2('map.csv')

base_camacol <- read.csv2('camacol.csv')
base_camacol$Area_m2 <- as.numeric(base_camacol$Area_m2)

base_camacol$Estrato <- as.factor(base_camacol$Estrato)
#mapa = cbind(mapa[c(1,2,3,6,7,8)], GNresult[c(4,5)])

logo <- tags$a(href='https://www.camacol.co',
               tags$img(src="logo_camacol.png", height = '50', width = '50'),
               'Proyectos de Vivienda Nuevos', target="_blank")



ui <- dashboardPage(
    dashboardHeader(title = logo, titleWidth = 600),
    dashboardSidebar(disable = T),
    dashboardBody(
        
        #_____________css___________________________
        tags$head(tags$style(HTML('
            .skin-blue .main-header .logo  { background-color: #222f4a;}
        
            .skin-blue .main-header .navbar {background-color: #222f4a;} 
        
            .main-header .logo {height:80px}
         
             .navbar {min-height:80px}
         
             a img { margin:10px;}
         
            a{ color:white; font-size:30px}
         
            .content-wrapper{background-color:#ebebf0; overflow:auto} 
            
            .box.box-solid.box-primary>.box-header {
                                         color:#fff;
                                         background:#222f4a
                                                   }

            .box.box-solid.box-primary{
                                        border-bottom-color:#222f4a;
                                        border-left-color:#222f4a;
                                        border-right-color:#222f4a;
                                        border-top-color:#222f4a;
                                      }

        
        
        '))),
        
        
        fluidRow(
            box(leafletOutput('mymap')),
            box(plotOutput('scatter',height = 300),
                selectInput('Ciudad', 'Ciudad', 
                            choices = c(unique(base_camacol$Ciudad),"Todos"),selected = "Todos", width = 300))
        ),
        box(width= 4,plotOutput('bar',height = 200), 
            div(style= "display:inline-block", selectInput('Categoria','Categoria', 
                                                           choices = c('Estrato', 'Banios', 'Habitaciones', 'Parqueadero'), width = 250)),
            div(style= "display:inline-block",  selectInput('barCiudad', 'Ciudad', 
                                                            choices = c(unique(base_camacol$Ciudad),"Todos"),selected = "Todos", width = 250))
        ),
        box(width = 8, status = 'primary', height  = 300, title = 'Predecir Valor', solidHeader = T,
            div(style= "display:inline-block", numericInput('Area','Area m2', value= 80,
                                                           min = 0, max = 1000, width = 200)),
            div(style= "display:inline-block", numericInput('bano','Baño', value = 3,
                                                           min = 0, max = 7, width = 200)),
            div(style= "display:inline-block", numericInput('estrato','Estrato', 
                                                            value = 3,
                                                            min = 0, max = 7,, width = 200)),
            div(style= "display:inline-block", numericInput('park','Parqueadero', 
                                                            value = 3,
                                                            min = 0, max = 7, width = 200)),
            div(style= "display:inline-block", numericInput('park','Parqueadero', 
                                                            value = 3,
                                                            min = 0, max = 7, width = 200)),
            div(style= "display:inline-block", numericInput('hab','Habitaciones', 
                                                            value = 3,
                                                            min = 0, max = 7, width = 200)),
            div(style= "display:inline-block", numericInput('dest','Destacado', 
                                                            value = 3,
                                                            min = 0, max = 7, width = 200)),
            div(style= "display:inline-block", numericInput('city','Ciudad', 
                                                           value = 400,
                                                           min = 0, max = 400, width = 200)),
            
            div(style= "display:inline-block", textOutput('precio'), width = 200)
            
                
                ))
    )

m<- as.data.frame(matrix(0,ncol = 7, nrow = 1))
colnames(m) <- c("Area_m2" ,     "Habitaciones", "Banios",       "Parqueadero",  "Estrato",      "Es_destacado", "Freq")

server <- function(input, output) {


    
    prediction <- reactive({
        
        
        data.frame(
         input$Area,
         input$hab,
        input$bano,
        input$park,
        input$estrato,
        input$dest,
        input$city
        )
        
       
    })

    saber = reactive({
       
         j = prediction()
       colnames(j) =c("Area_m2" ,     "Habitaciones", "Banios",       "Parqueadero",  "Estrato",      "Es_destacado", "Freq")
       d = round(predict(moda,j)) 
       return(paste('El valor del inmueble es:', d, 'millones de pesos'))
    })
 
 
 
      
    
    #___________mapa___________
    output$mymap <-  renderLeaflet({
        leaflet(mapa) %>% addProviderTiles(providers$CartoDB.Positron) %>%  
            addCircles(lng = ~Longitud, lat = ~Latitud, weight = 15,
                       radius = ~Freq*20, label = ~label)
    })
    
    #____________scatter____________    
    
    output$scatter <- renderPlot({
        
        if(input$Ciudad == "" || input$Ciudad == 'Todos') {
            ggplot(base_camacol, aes(x=Area_m2, y= Precio, color=Estrato)) + 
                geom_point(size=2) +  
                scale_y_continuous(limits = c(10000000, 1000000000)) + 
                scale_x_continuous(limits = c(0,200)) + ggtitle('Scatter Precio-Area')
        }
        else {
            ggplot(filter(base_camacol, Ciudad == input$Ciudad), aes(x=Area_m2, y= Precio, color=Estrato)) + 
                geom_point(size=2) + 
                scale_y_continuous(limits = c(10000000, 1000000000)) + 
                scale_x_continuous(limits = c(0,200)) + ggtitle('Scatter Precio-Area')
        }
    })
    
    #_________bar_________________
    
    output$bar <- renderPlot({
        if(input$barCiudad == "" || input$barCiudad == 'Todos') {
            ggplot(base_camacol, aes_string(x = input$Categoria))+
                geom_bar(color="black", fill="#428dc7", alpha = 0.8 ) 
                 
            
        }
        else{
            ggplot(filter(base_camacol, Ciudad == input$barCiudad), aes_string(x = input$Categoria))+
                geom_bar(color="black", fill="#428dc7", alpha = 0.8 ) 
                
            
        }
    })
    
   
    
    #_______precio__________
    output$precio <-renderPrint( { 
        
        saber()
    
})
}

shinyApp(ui = ui, server = server)
