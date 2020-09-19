
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Define server  -------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ModuloExtraServer = function(id, producto = id){
  
  moduleServer(id,
               # Empieza Servidor Modulo  ..................................
               function(input,output,session){
                 
                 #........................................................
                 # Archivo Subido y Calculo Primas  ......................
                 #........................................................
                 datos = reactive({
                   df0 = df_cartera
                   Tipo_interes = input$tipo_interes/100
                   try({
                     # shiny::req(input$archivo)
                     inFile = input$archivo
                     if(is.null(inFile)) cat("******** No hay archivo  *********")
                     
                     df0 = read_excel(inFile$datapath, 1)
                     
                   })
                   
                   df0 = limpieza_cartera(df0,Tipo_interes)
                   
                   return(df0)
                 })
                 
                 # Alerta Archivo subido
                 observeEvent(!is.null(input$archivo),
                 {
                   # print("fuera...")
                   show_alert(
                       title = "Carga Exitosa !!",
                       text = "Los datos se cargaron exitosamente.",
                       showCloseButton = TRUE,
                       type = "success"
                   )
                   
                 })
                 
                 
                 #............................................
                 # Tabla de DT Datos .........................
                 #............................................
                 output$tabla = renderDataTable({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   datatable(df0, rownames = F, 
                             extensions = c('FixedColumns','Scroller'),
                             options = list(
                               dom = 't',
                               pageLength = 10,
                               scrollX = TRUE,
                               fixedColumns = TRUE,
                               deferRender = TRUE,
                               scrollY = 200,
                               scroller = TRUE
                             ))
                 })
                 
                 # Grafico Variable Sexo  .....................
                 output$graf_cuantia = renderPlotly({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   df = df0
                   df$Edad = age(df$fecha_nacimiento)
                   
                   df %>%
                     plot_ly(x = ~Sexo,y = ~cuantia,
                             split = ~Sexo,type = 'violin',
                             box = list(visible = T),
                             meanline = list(visible = T)
                     ) %>%
                     layout(
                       xaxis = list(title = "Sexo"),
                       yaxis = list(
                         title = "Cuantía",
                         zeroline = F
                       ),
                       # Fondo Transparente
                       font=fuente,
                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)"#,
                       # fig_bgcolor   = "rgba(0, 0, 0, 0)"
                     )
                   
                   
                 })
                 
                 
                 # Grafico Variable Edad  .....................
                 output$graf_edad = renderPlotly({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   df = df0
                   df$Edad = age(df$fecha_nacimiento)
                   
                   df %>%
                     plot_ly(x = ~Sexo,y = ~Edad,
                             split = ~Sexo,type = 'violin',
                             box = list(visible = T),
                             meanline = list(visible = T)
                     ) %>%
                     layout(
                       xaxis = list(title = "Sexo"),
                       yaxis = list(
                         title = "Edad",
                         zeroline = F
                       ),
                       # Fondo Transparente
                       font=fuente,
                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)"#,
                       # fig_bgcolor   = "rgba(0, 0, 0, 0)"
                     )
                   
                   
                 })
                 
                 # Duracion vs Prima Pura .................
                 output$graf_dura_cuant = renderHighchart({
                   
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   hc = df0 %>% 
                     hchart('scatter', hcaes(x = duracion, y = prima_pura, group = Sexo),regression = TRUE) %>%
                     hc_colors(c("#00AFBB", "#E7B800"))%>% 
                     hc_add_dependency("plugins/highcharts-regression.js")
                 })
                 
                 # Gastos Internos vs Externos  ..............
                 output$graf_prima_edad = renderHighchart({
                   
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   hc = df0 %>% 
                     hchart('scatter', hcaes(x = Edad, y = prima_pura, group = Sexo),regression = TRUE) %>%
                     hc_colors(c("#00AFBB", "#E7B800"))%>% 
                     hc_add_dependency("plugins/highcharts-regression.js")
                 })
                 
                 
                 
                 
                 
                 #............................................
                 # Resultado Primas x Producto  ..............
                 #............................................
                 output$graf_prima_prod = renderPlotly({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_pura
                   prima = sum(prima,na.rm=TRUE)
                   
                   df0 %>%  
                     group_by(Producto) %>% 
                     summarise(Prima_pura = sum(prima_pura,na.rm = TRUE),
                               Prima_inventario = sum(prima_inventario,na.rm = TRUE),
                               Prima_comercial = sum(prima_comercial,na.rm = TRUE),
                               Prima_fraccionada = sum(prima_fraccionada,na.rm = TRUE),
                               Prima_nivelada = sum(prima_nivelada,na.rm = TRUE)) %>% 
                     plot_ly(x = ~Producto, y = ~Prima_pura, 
                             type = 'bar', name = 'Prima pura') %>%
                     add_trace(y = ~Prima_inventario, name = 'Prima inventario') %>%
                     add_trace(y = ~Prima_comercial, name = 'Prima comercial') %>%
                     add_trace(y = ~Prima_fraccionada, name = 'Prima fraccionada') %>%
                     add_trace(y = ~Prima_nivelada, name = 'Prima nivelada') %>%
                     layout(yaxis = list(title = 'Primas Agregadas'), barmode = 'stack',
                            # Fondo Transparente
                            font=fuente,
                            plot_bgcolor  = "rgba(0, 0, 0, 0)",
                            paper_bgcolor = "rgba(0, 0, 0, 0)"#,
                            # fig_bgcolor   = "rgba(0, 0, 0, 0)"
                     )
                   
                 })
                 
                 
                 #............................................
                 # Box de Resultado Primas ...................
                 #............................................
                 output$box_prima_pura = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_pura
                   prima = sum(prima,na.rm=TRUE)
                   # print(prima)
                   titulo = "Prima Pura"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'blue'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = TRUE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 #....
                 output$box_prima_inventario = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_inventario
                   prima = sum(prima,na.rm=TRUE)
                   titulo = "Prima inventario"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'yellow'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = TRUE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 #....
                 output$box_prima_comercial = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_comercial
                   prima = sum(prima,na.rm=TRUE)
                   titulo = "Prima comercial"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'green'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = TRUE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 # .............................................
                 # Primas opcionales ...........................
                 # .............................................
                 output$box_prima_fraccionada = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_fraccionada
                   prima = sum(prima,na.rm=TRUE)
                   titulo = "Prima fraccionada"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'red'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = FALSE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 #....
                 output$box_prima_nivelada = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_nivelada
                   prima = sum(prima,na.rm=TRUE)
                   titulo = "Prima nivelada"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'purple'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = FALSE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 
                 
                 
                 
                 #.....................................................
                 # Box de Resultado Primas PROMEDIO ...................
                 #.....................................................
                 output$box_prima_pura_prom = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_pura
                   prima = mean(prima,na.rm=TRUE)
                   # print(prima)
                   titulo = "Prima Pura"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'blue'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = TRUE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 #....
                 output$box_prima_inventario_prom = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_inventario
                   prima = mean(prima,na.rm=TRUE)
                   titulo = "Prima inventario"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'yellow'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = TRUE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 #....
                 output$box_prima_comercial_prom = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_comercial
                   prima = mean(prima,na.rm=TRUE)
                   titulo = "Prima comercial"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'green'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = TRUE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 # .............................................
                 # Primas opcionales ...........................
                 # .............................................
                 output$box_prima_fraccionada_prom = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_fraccionada
                   prima = mean(prima,na.rm=TRUE)
                   titulo = "Prima fraccionada"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'red'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = FALSE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 #....
                 output$box_prima_nivelada_prom = renderInfoBox({
                   df0 = df_cartera
                   # shiny::req(input$archivo)
                   if(!is.null(input$archivo$datapath)) df0 = datos()
                   
                   prima = df0$prima_nivelada
                   prima = mean(prima,na.rm=TRUE)
                   titulo = "Prima nivelada"
                   if(is.null(prima)){
                     prima = 'No disponible'; color = 'black'
                   }else{
                     prima = round(prima,2)
                     color = 'purple'
                   }
                   try({
                     caja = infoBox(title = titulo, fill = FALSE,
                                    value = prima,
                                    icon = icon("stats", lib = "glyphicon"),color=color)
                   })
                   return(caja)
                 })
                 
                 
               }
  )
}