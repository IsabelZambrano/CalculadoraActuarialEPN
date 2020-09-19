
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Define UI -------------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ModuloUI = function(id = "i_producto", titulo="Titulo_Pestania"){
  
  ns = NS(id)
  #............................................................
  # Panel Lateral .............................................
  #............................................................
  tabItem(tabName = id,
          h3(titulo), #hr(),
          
          fluidRow(
            # PANEL DE PARAMETROS ............
            # sidebarPanel( # Se cambio por box()
            box(title = "Parámetros",status = "primary",
                solidHeader = FALSE,collapsible = TRUE,
                
                # PorUp Informacion del Producto ..............
                actionBttn(
                  inputId = ns("informacion"),
                  label = "Guía de Usuario",
                  style = "jelly", #"pill",'unite','material-flat','minimal','stretch','fill'
                  icon = icon('info'),
                  color = "primary"
                ),br(),br(),
                
                
                # Contenedor Parametros
                fluidRow(
                  
                  column(6,
                         # Tipo de Seguro
                         uiOutput(outputId = ns("wid_tipo_seguro")),
                         
                         # Tipo de Renta
                         uiOutput(outputId = ns("wid_tipo_renta")),
                         
                         # Widgets COMUNES ................................
                         
                         # Fecha de Nacimiento 
                         dateInput(inputId = ns('edad'),
                                   label = 'Fecha de nacimiento:', 
                                   value = as.character(Sys.Date()-18*365), 
                                   min = as.character(Sys.Date()-98*365), 
                                   max = as.character(Sys.Date()-18*365), 
                                   format = "yyyy-mm-dd", 
                                   startview = "decade", 
                                   weekstart = 0, language = "es", width='90%'),
                         # Sexo
                         radioButtons(inputId = ns('sexo'),
                                      label = 'Sexo:', 
                                      choices = c('Masculino'= 'Hombre', 'Femenino'='Mujer'), 
                                      selected = NULL, width='90%'),
                         # Tipo de interes
                         sliderInput(inputId = ns('tipo_interes'), 
                                     label = 'Tipo de interés:', 
                                     min = 0, max = 100, 
                                     value = 6, 
                                     step = 0.1, round = FALSE, 
                                     #format = "#,##0.#####",  
                                     post  = " %",
                                     # locale = "us", 
                                     ticks = TRUE, 
                                     animate = FALSE, 
                                     width='90%'),
                         
                         # Gastos Internos
                         sliderInput(inputId = ns('gasto_int'), 
                                     label = 'Gastos Internos:', 
                                     min = 0, max = 100, 
                                     value = 4, 
                                     step = 0.1, round = FALSE, 
                                     #format = "#,##0.#####",  
                                     post  = " %",
                                     # locale = "us", 
                                     ticks = TRUE, animate = FALSE, 
                                     width='90%'),
                         # Gastos Internos
                         sliderInput(inputId = ns('gasto_ext'), 
                                     label = 'Gastos Externos:', 
                                     min = 0, max = 100, 
                                     value = 4, 
                                     step = 0.1, round = FALSE, 
                                     #format = "#,##0.#####",  
                                     post  = " %",
                                     # locale = "us", 
                                     ticks = TRUE, animate = FALSE, 
                                     width='90%'),
                         
                         # Cuantia
                         numericInput(inputId = ns('cuantia'), 
                                      label = 'Cuantía:', 
                                      value = 1000, min = 0, step = NA, width='90%')
                         
                         
                  ),
                  
                  # Columna 2 ............................................
                  column(6,
                         
                         # Duracion Prestacion
                         uiOutput(outputId = ns("wid_duracion")),
                         # Fraccionar
                         selectInput(inputId = ns('tipo_fraccion'),
                                     label='Desea Fraccionar la Prima:',
                                     choices = c('Si',
                                                 'No'),
                                     selected = 'No',
                                     width='90%'),
                         
                         # Actualizar Widget Temporalidad
                         uiOutput(outputId = ns("wid_temporalidad")),
                         # Actualizar Widget Cantidad Fraccion
                         uiOutput(outputId = ns("wid_fraccion")),
                         # hr(),
                         
                         
                         # Crecimiento Prestacion
                         uiOutput(outputId = ns("wid_tipo_crecim")),
                         
                         # Actualizar en Server segun tipo:
                         uiOutput(outputId = ns("wid_crecimiento")),
                         
                         # Diferimiento en Anios
                         uiOutput(outputId = ns("wid_diferido"))
                  )
                )
                
            ),
            
            #............................................................
            # Panel Principal ...........................................
            #............................................................
            # mainPanel(
            box(title = "Primas",status = "success",
                solidHeader = F,collapsible = TRUE,
                # Cajas con Primas
                fluidRow(
                  infoBoxOutput(outputId = ns('box_prima_pura')),
                  infoBoxOutput(outputId = ns('box_prima_inventario')),
                  infoBoxOutput(outputId = ns('box_prima_comercial')) 
                ),
                fluidRow(
                  infoBoxOutput(outputId = ns('box_prima_fraccionada')),
                  infoBoxOutput(outputId = ns('box_prima_nivelada'))
                ),
                # Grafico Reserva
                uiOutput(ns('wid_reserva'))
            ),
            box(title = "Calculos",status = "warning", collapsed = TRUE,
                #c('primary','success','info','warning','danger')
                solidHeader = F,collapsible = TRUE,
              # Print Resultados
              fluidRow(
                verbatimTextOutput(outputId = ns('print_resultados'))
              )
            )
            
          )#,hr()
          
  )
  
  
  
  
  
}

