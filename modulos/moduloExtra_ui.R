
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Define UI -------------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ModuloExtraUI = function(id = "i_producto", titulo="Titulo_Pestania"){
  
  ns = NS(id)
  #............................................................
  # Tabla de Datos ............................................
  #............................................................
  tabItem(tabName = id,
          h3(titulo),
          
          
          fluidRow(
            
            
            # CAJA  ......................................
            box(title = "Datos Cartera",status = "info",
                solidHeader = FALSE,collapsible = TRUE,
                
                # Subida Archivos
                fileInput(inputId = ns("archivo"), 
                          "Subir archivo (.xlsx):",
                          buttonLabel = 'Buscar',
                          placeholder = 'Ingresa un archivo .xlsx',
                          # multiple = FALSE,
                          accept = c(".xlsx")),
                
                # Tipo de interes
                sliderInput(inputId = ns('tipo_interes'), 
                            label = 'Tipo de interés', 
                            min = 0, max = 100, 
                            value = 6, 
                            step = 0.1, round = FALSE, 
                            #format = "#,##0.#####",  
                            post  = " %",
                            # locale = "us", 
                            ticks = TRUE, 
                            animate = FALSE, 
                            width='90%'),
                
                # Tabla Datos ...................
                dataTableOutput(outputId = ns("tabla"))  
            ),
            
            
            
            
            # CAJA PRIMAS AGREGADAS   ....................
            box(title = "PRIMAS: Valores Agregados Primas",status = "success",
                solidHeader = FALSE,collapsible = TRUE,
                # h3('cccc'),
                fluidRow(
                  infoBoxOutput(outputId = ns('box_prima_pura')),
                  infoBoxOutput(outputId = ns('box_prima_inventario')),
                  infoBoxOutput(outputId = ns('box_prima_comercial')) 
                ),
                fluidRow(
                  infoBoxOutput(outputId = ns('box_prima_fraccionada')),
                  infoBoxOutput(outputId = ns('box_prima_nivelada'))
                )
            ),
            
            # CAJA PRIMAS PROMEDIO   ....................
            box(title = "PRIMAS: Valores Promedio ",status = "success",
                solidHeader = FALSE,collapsible = TRUE,
                # h3('cccc'),
                fluidRow(
                  infoBoxOutput(outputId = ns('box_prima_pura_prom')),
                  infoBoxOutput(outputId = ns('box_prima_inventario_prom')),
                  infoBoxOutput(outputId = ns('box_prima_comercial_prom')) 
                ),
                fluidRow(
                  infoBoxOutput(outputId = ns('box_prima_fraccionada_prom')),
                  infoBoxOutput(outputId = ns('box_prima_nivelada_prom'))
                )
            ),
            
            # Grafica PRIMAS AGREGADAS   ....................
            box(title = "PRIMAS: Valores Agregados Primas",status = "success",
                solidHeader = FALSE,collapsible = TRUE,
                
                plotlyOutput(outputId = ns('graf_prima_prod'))
                
            ),
            
            
            
            # CAJA  ......................................
            box(title = "Cuantía y Edad vs Sexo",status = "info",
                solidHeader = FALSE,collapsible = TRUE,
                # h3('bbbb'),
                fluidRow(
                  column(6,
                         plotlyOutput(outputId = ns('graf_cuantia'))
                  ),
                  column(6,
                         plotlyOutput(outputId = ns('graf_edad'))
                  )
                )
            ),
            
            
            # CAJA  ......................................
            box(title = "Duración vs Cuantía",status = "info",
                solidHeader = FALSE,collapsible = TRUE,
                # h3('dddd')
                highchartOutput(outputId = ns('graf_dura_cuant'))
            ),
            # CAJA  ......................................
            box(title = "Gastos Internos vs Externos",status = "info",
                solidHeader = FALSE,collapsible = TRUE,
                # h3('dddd')
                highchartOutput(outputId = ns('graf_gastos'))
            )
            
            
          )
          
          
          
  )
}