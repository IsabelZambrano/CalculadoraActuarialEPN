#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Funciones Extras -------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Letra Color Plotly  .................
fuente = list(color = 'white')#,# family = "sans serif",size = 14)

# Letra HighCharter  ..................
thmHC = hc_theme(colors = c('red','green','blue'),
                 chart = list(backgroundColor = "#15C0DE"),
                 title = list(style = list(color ='#ffffff',
                                           fontFamily = "Erica One")),
                 subtitle = list(style = list(color ='#ffffff',
                                              fontFamily = "Shadows Into Light")),
                 legend = list(itemStyle = list(fontFamily ='Tangerine',color ='#ffffff')
                               ,itemHoverStyle = list(color ='#ffffff')))


# Edad  ..............................
age = function(dob, age.day = today(), units = "years", floor = TRUE) {
  require(lubridate)
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

# Reserva ....................
Reserva = function(producto,t,l,tabla,Cuantia,Edad,Duracion,Tipo_interes,Diferido){
  
  PNivel = l$prima_nivelada
  reserva = switch (producto,
                    '1_fallecimiento' = {
                      Cuantia*Axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=Diferido, k=1) - 
                        PNivel*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=Diferido, payment = "immediate", k=1)
                    },
                    '1_supervivencia' = {
                      Cuantia*Exn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes) - 
                        PNivel*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=Diferido, payment = "immediate", k=1)
                    },
                    '1_mixto' = {
                      Cuantia*AExn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, k=1) - 
                        PNivel*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=Diferido, payment = "immediate", k=1)
                    },
                    '1_diferido' = {
                      Cuantia*Axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=Diferido, k=1) - 
                        PNivel*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=Diferido, payment = "immediate", k=1)
                    },
                    '2_prepagables' = {
                      Cuantia*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=0, k=1, payment = "due") - 
                        PNivel*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=0, payment = "immediate", k=1)
                    },
                    '2_pospagables' = {
                      Cuantia*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=0, k=1, payment = "immediate") - 
                        PNivel*axn(tabla, x=Edad+t, n=Duracion-t, i=Tipo_interes, m=0, payment = "immediate", k=1)
                    }
                    
  )
  return(reserva)
} 



# Calculos Limpieza df_cartera ....................................................
limpieza_cartera = function(df0,Tipo_interes){
  df0$fraccion = as.numeric(df0$fraccion)
  df0$Edad = age(df0$fecha_nacimiento)
  
  df0$Sexo = NA
  df0$Sexo[df0$sexo=='M'] = 'Hombre'
  df0$Sexo[df0$sexo=='F'] = 'Mujer'
  
  df0$Producto = NA
  df0$Producto[df0$producto==1] = "1_fallecimiento"
  df0$Producto[df0$producto==2] = "1_supervivencia"
  df0$Producto[df0$producto==3] = "1_mixto"
  df0$Producto[df0$producto==4] = "1_cuantia_variable"
  
  df0$Temporalidad = NA
  df0$Temporalidad[df0$temporalidad=='mensual'] = 12
  df0$Temporalidad[df0$temporalidad=='trimestral'] = 4
  df0$Temporalidad[df0$temporalidad=='semestral'] = 2
  
  df0$Seleccion_frac = NA
  df0$Seleccion_frac[df0$seleccion_fraccion=='si'] = 'Si'
  df0$Seleccion_frac[df0$seleccion_fraccion=='no'] = 'No'
  
  df0$prima_pura = NA
  df0$prima_inventario = NA
  df0$prima_comercial = NA
  df0$prima_fraccionada = NA
  df0$prima_nivelada = NA
  # Calculo de Primas .............
  for(i in seq(dim(df0)[1])){
    # cat("********** Limpieza ID:", df0$id[i])
    resultado_primas = NULL
    try({
      resultado_primas = calculo_producto(producto = df0$Producto[i],
                                          Edad = df0$Edad[i], Sexo = df0$Sexo[i],
                                          Tipo_interes = Tipo_interes,
                                          Duracion = df0$duracion[i],
                                          Cuantia = df0$cuantia[i],
                                          Tipo_crecimiento = 'Geometrico',
                                          Crecimiento = Tipo_interes,  # OJO solo es parche
                                          Gastos_internos = df0$gastos_internos[i],
                                          Gastos_externos = df0$gastos_externos[i],
                                          Temporalidad = df0$Temporalidad[i],
                                          Seleccion_frac = df0$Seleccion_frac[i],
                                          Fraccion = df0$fraccion[i],
                                          Tipo_seguro = 'Entera',
                                          Diferido = 2)
      
    })
    
    try({if(!is.null(resultado_primas$prima_pura)) df0$prima_pura[i] = resultado_primas$prima_pura})
    try({if(!is.null(resultado_primas$prima_inventario)) df0$prima_inventario[i] = resultado_primas$prima_inventario})
    try({if(!is.null(resultado_primas$prima_comercial)) df0$prima_comercial[i] = resultado_primas$prima_comercial})
    try({if(!is.null(resultado_primas$prima_fraccionada)) df0$prima_fraccionada[i] = resultado_primas$prima_fraccionada})
    try({if(!is.null(resultado_primas$prima_nivelada)) df0$prima_nivelada[i] = resultado_primas$prima_nivelada})
  }
  return(df0)
}


