library(shiny)
# library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
# Graficos y tablas
library(highcharter)
library(plotly)
library(DT)
# Calculos
library(lifecontingencies)
library(lubridate)
library(readxl)
library(dplyr)

load('datos/tablas_listas.RData')

source(file = 'modulos/modulo_server.R',local = T)
source(file = 'modulos/modulo_ui.R',local = T)
source(file = 'modulos/moduloExtra_server.R',local = T)
source(file = 'modulos/moduloExtra_ui.R',local = T)
source(file = 'modulos/modulo_id.R',local = T)
source(file = 'codigo/calculos.R',local = T)
source(file = 'codigo/extras.R',local = T)
source(file = 'codigo/info.R',local = TRUE)

df_cartera = read_excel(path = 'datos/Ejemplo_Cartera.xlsx')
df_cartera = limpieza_cartera(df_cartera,Tipo_interes = 0.06)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Define UI -------------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ui = dashboardPagePlus(
    title = "Calculadora actuarial",
    # ENCABEZADO ...............................
    dashboardHeaderPlus(
        title = "Calculadora actuarial",
        dropdownMenu(type = "messages",
                     messageItem(
                         from = prsnl(1),
                         message = ""
                     ),
                     messageItem(
                         from = prsnl(2),
                         message = ""
                     ),
                     messageItem(
                         from = prsnl(3),
                         message = ""
                     ),
                     messageItem(
                         from = prsnl(4),
                         message = ""
                     )
        )
    ),
    
    # PESTANIAS MENU ...........................
    dashboardSidebar(
        sidebarMenu(
            
            # SEGUROS
            menuItem("Seguros de Vida",tabName = "seguros_vida",icon = icon("list"),
                     menuSubItem('Fallecimiento',tabName = '1_fallecimiento',icon = icon('line-chart')),
                     menuSubItem('Supervivencia',tabName = '1_supervivencia',icon = icon('line-chart')),
                     menuSubItem('Mixto',tabName = '1_mixto',icon = icon('line-chart')),
                     menuSubItem('Diferido',tabName = '1_diferido',icon = icon('line-chart')),
                     menuSubItem('Cuantía Variable',tabName = '1_cuantia_variable',icon = icon('line-chart'))
            ),
            # RENTAS
            menuItem("Rentas",tabName = "rentas",icon = icon("list"),
                     menuSubItem('Prepagables',tabName = '2_prepagables',icon = icon('line-chart')),
                     menuSubItem('Pospagables',tabName = '2_pospagables',icon = icon('line-chart'))
            ),
            # EXTRAS
            menuItem("Calculadora Cartera",tabName = "3_cartera",icon = icon("line-chart"))#,
            
            # Integrantes
            # menuItem("Integrantes",tabName = '4_integrantes', icon = icon('users'))
            
        )
    ),
    
    # CUERPO  .........................................
    dashboardBody(
        # Tema del Dashboard  .........................
        shinyDashboardThemes(theme = "grey_dark"),
        # c('blue_gradient','flat_red','grey_light','grey_dark','onenote','poor_mans_flatly','purple_gradient')
        
        tabItems(
            
            # Fallecimiento ...........................
            ModuloUI(id='1_fallecimiento',titulo='Seguros de Vida en Caso de Fallecimiento'),
            # Supervivencia  ..............................
            ModuloUI(id='1_supervivencia',titulo='Seguros de Vida en Caso de Supervivencia'),
            # Mixto  ..............................
            ModuloUI(id='1_mixto',titulo='Seguros de Vida Mixtos'),
            # Diferido  ..............................
            ModuloUI(id='1_diferido',titulo='Seguros de Vida Diferidos'),
            # Cuantia Variable   .......................
            ModuloUI(id='1_cuantia_variable',titulo='Seguro de Cuantía Variable'),
            
            
            # Prepagables .......................
            ModuloUI(id='2_prepagables',titulo='Rentas Actuariales Prepagables'),
            # Pospagables .......................
            ModuloUI(id='2_pospagables',titulo='Rentas Actuariales Pospagables'),
            
            
            # Calculadora Cartera ..............
            ModuloExtraUI(id='3_cartera',titulo='Calculadora Cartera')#,
            
            
            # Perfil Integrantes  .............
            # ModuloIdUI(id='4_integrantes',titulo = 'Integrantes')
            
            
        )
        
    )
)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Define server  -------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
server = function(input, output, session) {
    
    # Calculos Seguros de Vida ..........
    ModuloServer(id = '1_fallecimiento')
    ModuloServer(id = '1_supervivencia')
    ModuloServer(id = '1_mixto')
    ModuloServer(id = '1_diferido')
    ModuloServer(id = '1_cuantia_variable')
    
    # Calculos de Rentas
    ModuloServer(id = '2_prepagables')
    ModuloServer(id = '2_pospagables')
    
    # Extra cartera
    ModuloExtraServer(id='3_cartera')
}


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Run APP   -------------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
shinyApp(ui = ui, server = server)
