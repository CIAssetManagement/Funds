library(shiny)
library(ggplot2)

fondos <- read.csv("Fondos.csv",header = TRUE)
namesfondos <- as.character(unique(fondos$Fondo))
precios <- read.csv("Precios.csv",header = TRUE)
precios <- cbind(precios,Tipo = sub("-.*","",precios$Instrumento))


fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Roboto+Slab');
                    .shiny-output-error-validation {color: #FEFEFE; font-size: 150%}"),
                    '#sidebar{font-family:"Roboto Slab", serif;color: #fefefe;background-color:#16620a} 
                    body{background-color:#4fa22a')) ,
  
  headerPanel(h1("Simulador de operaciones", 
                 style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; 
                 color: #fefefe;")),
  
  sidebarPanel(
    
    id="sidebar",
    
    selectInput('fondo', 'Fondos', namesfondos),
    
    #Venta de instrumentos
      selectizeInput('instrumentov',label ='Venta de Instrumento',choices = NULL),
      div(style="display:inline-block",numericInput('montov','Monto', value = 0,min = 0)),
      div(style="display:inline-block",numericInput('titulosv','Títulos', value = 0, min = 0)),
    
    #Boton para agregar los valores de venta a la tabla.
    actionButton("addv","Agregar",),

    #Compra de instrumentos 
    selectizeInput('instrumentoc',label ='Compra de Instrumento',choices = NULL),
    div(style="display:inline-block",numericInput('montog','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosg','Títulos', value = 0, min = 0)),

    #Boton para agregar los valores de compra a la tabla.
    actionButton("addc","Agregar"),
    actionButton("deleteSelected", "Eliminar"),
    
    #Tabla instrumentos venta 
    h4('Venta de Instrumentos',style="margin:20px 0 0 0;"),
    div(style="align:left; position:relative; top:20px;",tableOutput('ventav')),
    #Tabala instrumentos compra
    h4('Compra de Instrumentos',style="margin:50px 0 0 0;"),
    div(style="align:left; position:relative; top:20px;",tableOutput('comprac'))),
  
  mainPanel(
    h2("Indicadores",style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; 
       color: #fefefe;"),
    DT::dataTableOutput("inda"),
    DT::dataTableOutput("indd"),
    verbatimTextOutput("inddx")
  )
)

