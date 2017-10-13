library(shiny)
library(ggplot2)

fondos <- read.csv("Fondos.csv",header = TRUE)
namesfondos <- as.character(unique(fondos$Fondo))

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
    
    titlePanel("Selección de fondos"),
    selectInput('fondo','', namesfondos),
    
    #Venta de instrumentos
    titlePanel("Venta de Instrumentos"),
    selectizeInput('TipoValorv',label='Tipo de Valor',choices = NULL),
    selectizeInput('Emisorav',label='Emisora',choices = NULL),
    selectizeInput('instrumentov',label ='Instrumento',choices = NULL),
    div(style="display:inline-block",numericInput('montov','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosv','Títulos', value = 0, min = 0)),
    
    #Boton para agregar los valores de venta a la tabla.
    actionButton("addv","Agregar"),

    #Compra de instrumentos 
    titlePanel("Compra de Instrumentos"),
    selectizeInput('TipoValorc',label='Tipo de Valor',choices = NULL),
    selectizeInput('Emisorac',label='Emisora',choices = NULL),
    selectizeInput('instrumentoc',label ='Instrumento',choices = NULL),
    div(style="display:inline-block",numericInput('montog','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosg','Títulos', value = 0, min = 0)),

    #Boton para agregar los valores de compra a la tabla.
    actionButton("addc","Agregar"),
    
    #Tabla instrumentos venta 
    h4('Venta de Instrumentos',style="margin:20px 0 0 0;"),
    div(style="align:left; position:relative; top:20px;",tableOutput('ventav')),
    #Tabala instrumentos compra
    h4('Compra de Instrumentos',style="margin:50px 0 0 0;"),
    div(style="align:left; position:relative; top:20px;",tableOutput('comprac')),
    actionButton("summit","Cargar")),
    #actionButton("limpiar", "Limpiar")),
    
  
  fluidRow(
    checkboxGroupInput("show_vars","Selecciona el Fondo:",namesfondos,namesfondos,inline=TRUE),
    column(4,h2("Fondos",style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #fefefe;"),hr(),
    DT::dataTableOutput("funda"),
    h2("Medidas de Riesgo",style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1;color: #fefefe;"),hr(),
    DT::dataTableOutput("inda")),
  fluidRow(column(4,h2("Fondos Operaciones",style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #fefefe;"),hr(),
    DT::dataTableOutput("fundd"),
    h2("Medidas de Riesgo Simulación",style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #fefefe;"),hr()),
    DT::dataTableOutput("indd"))
  )
  
)

