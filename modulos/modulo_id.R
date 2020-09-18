#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ModuloIdUI = function(id = "i_producto", titulo="Titulo_Pestania"){
  ns = NS(id)
  #............................................................
  # Panel Cajas   .............................................
  #............................................................
  tabItem(tabName = id,
          h3(titulo), #hr(),
          
          fluidRow(
            
            # Perfil1 .........................
            column(3,
            widgetUserBox(
              title = "Elizabeth Pierce",
              subtitle = "Web Designer",
              type = NULL,
              width = 12,
              src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
              background = TRUE,
              backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
              closable = TRUE,
              "Some text here!",
              footer = "The footer here!"
            )),
            
            # Perfil2 .........................
            column(3,
            widgetUserBox(
              title = "Elizabeth Pierce",
              subtitle = "Web Designer",
              type = NULL,
              width = 12,
              src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
              background = TRUE,
              backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
              closable = TRUE,
              "Some text here!",
              footer = "The footer here!"
            )),
            
            # Perfil3 .........................
            column(3,
            widgetUserBox(
              title = "Elizabeth Pierce",
              subtitle = "Web Designer",
              type = NULL,
              width = 12,
              src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
              background = TRUE,
              backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
              closable = TRUE,
              "Some text here!",
              footer = "The footer here!"
            )),
            
            # Perfil4 .........................
            column(3,
            widgetUserBox(
              title = "Elizabeth Pierce",
              subtitle = "Web Designer",
              type = NULL,
              width = 12,
              src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
              background = TRUE,
              backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
              closable = TRUE,
              "Some text here!",
              footer = "The footer here!"
            ))
            
          )
  )
  
}

