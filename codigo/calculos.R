# Funcion de Calculo de Primas ............................................
calculo_producto = function(producto,Tipo_seguro,Edad,Sexo,Tipo_interes,
                            Gastos_internos,Gastos_externos,Cuantia,Duracion,
                            Seleccion_frac,Temporalidad,Fraccion,
                            Tipo_crecimiento,Crecimiento,Diferido,...){
  require(lifecontingencies)
  respuesta = 
    switch (producto,
            #...............................#...............................
            # Seguros de Vida  .............#...............................
            #...............................#...............................
            '1_fallecimiento' = {
              l = list()
              tabla = switch(Sexo,'Hombre' = {TH},'Mujer' = {TM})
              #...............................
              if(Tipo_seguro =='Entera'){
                if(Seleccion_frac=='No') {
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, i = Tipo_interes, k=1)
                  l$prima_inventario = l$prima_pura*(1+Gastos_internos)
                  l$prima_comercial = l$prima_pura*(1+Gastos_internos)*(1+Gastos_externos)
                }
                if(Seleccion_frac=='Si'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, i = Tipo_interes, k=1)
                  l$prima_fraccionada = (l$prima_pura/axn(tabla, x=Edad, n = Fraccion , 
                                                          i=Tipo_interes, m=0, 
                                                          k=Temporalidad ,payment = "due"))/Temporalidad
                  l$prima_inventario = l$prima_fraccionada*(1+Gastos_internos)
                  l$prima_comercial = l$prima_inventario*(1+Gastos_externos)
                }
              }
              if(Tipo_seguro=='Temporal'){
                
                if(Seleccion_frac=='No'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, n = Duracion, i = Tipo_interes, k=1)
                  l$prima_inventario = l$prima_pura*(1+Gastos_internos)
                  l$prima_comercial = l$prima_inventario*(1+Gastos_externos)
                  l$prima_nivelada = l$prima_pura/axn(tabla, x=Edad, n = Duracion, i = Tipo_interes, m = 0, k = 1, payment = "due")
                  l$reserva = c()
                  for(t in 0:Duracion){
                    l$reserva[t+1] = #paste0("Anio : ", t, " Reserva Matematica : ", 
                      Reserva(producto=producto,t=t,l=l,tabla=tabla,Cuantia=Cuantia,
                              Edad=Edad,Duracion=Duracion,
                              Tipo_interes=Tipo_interes,Diferido=0)
                    #)
                  }
                  # plot(seq(0,Duracion), l$reserva, type='l', col=3, xlab='Anio', ylab='Reserva')
                }
                if(Seleccion_frac=='Si'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, n= Duracion, i = Tipo_interes, k=1)
                  l$prima_fraccionada = (l$prima_pura/axn(tabla, x=Edad, n = Fraccion , i=Tipo_interes, m=0, k=Temporalidad ,payment = "due"))/Temporalidad
                  l$prima_inventario = l$prima_fraccionada*(1+Gastos_internos)
                  l$prima_comercial = l$prima_inventario*(1+Gastos_externos)
                }
              }
              
              return(l)
            },
            #...............................
            #...............................
            '1_supervivencia' = {
              l = list()
              tabla = switch(Sexo,'Hombre' = {TH},'Mujer' = {TM})
              #...............................
              if(Seleccion_frac=='No'){
                l$prima_pura = Cuantia*Exn(tabla, x = Edad, n = Duracion, i = Tipo_interes)
                l$prima_inventario = l$prima_pura*(1+Gastos_internos)
                l$prima_comercial = l$prima_inventario*(1+Gastos_externos)
                l$prima_nivelada = l$prima_pura/axn(tabla, x=Edad, n = Duracion, i = Tipo_interes, m = 0, k = 1, payment = "due")
                l$reserva = c()
                for(t in 0:Duracion){
                  l$reserva[t+1] = #paste0("Anio : ", t, " Reserva Matematica : ", 
                    Reserva(producto=producto,t=t,l = l,tabla=tabla,Cuantia=Cuantia,
                            Edad=Edad,Duracion=Duracion,
                            Tipo_interes=Tipo_interes,Diferido=0)
                  #)
                }
                # plot(seq(0,Duracion), l$reserva, type='l', col=3, xlab='Anio', ylab='Reserva')
                
              }
              if(Seleccion_frac == 'Si'){
                l$prima_pura = Cuantia*Exn(tabla, x = Edad, n= Duracion, i = Tipo_interes)
                l$prima_fraccionada =  (l$prima_pura/axn(tabla, x=Edad, n = Fraccion , i=Tipo_interes, m=0, k=Temporalidad ,payment = "due"))/Temporalidad
                l$prima_inventario = l$prima_fraccionada*(1+Gastos_internos)
                l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
              }
              
              return(l)
            },
            #...............................
            #...............................
            '1_mixto' = {
              l = list()
              tabla = switch(Sexo,'Hombre' = {TH},'Mujer' = {TM})
              #...............................
              if(Seleccion_frac=='No'){
                l$prima_pura = Cuantia*AExn(tabla, x = Edad, n = Duracion, i = Tipo_interes, k=1)
                l$prima_inventario  = l$prima_pura*(1+Gastos_internos)
                l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
                l$prima_nivelada = l$prima_pura/axn(tabla, x=Edad, n = Duracion, i = Tipo_interes, m = 0, k = 1, payment = "due")
                l$reserva = c()
                for(t in 0:Duracion){
                  l$reserva[t+1] = #paste0("Anio : ", t, " Reserva Matematica : ", 
                    Reserva(producto=producto,t=t,l = l,tabla=tabla,Cuantia=Cuantia,
                            Edad=Edad,Duracion=Duracion,
                            Tipo_interes=Tipo_interes,Diferido=0)
                  #)
                }
                # plot(seq(0,Duracion), l$reserva, type='l', col=3, xlab='Anio', ylab='Reserva')
                
              }
              if(Seleccion_frac=='Si'){
                l$prima_pura = Cuantia*AExn(tabla, x = Edad, n= Duracion, i = Tipo_interes, k=1)
                l$prima_fraccionada =  (l$prima_pura/axn(tabla, x=Edad, n = Fraccion , i=Tipo_interes, m=0, k=Temporalidad ,payment = "due"))/Temporalidad
                l$prima_inventario = l$prima_fraccionada*(1+Gastos_internos)
                l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
              }
              return(l)
            },
            #...............................
            #...............................
            '1_diferido' = {
              l = list()
              tabla = switch(Sexo,'Hombre' = {TH},'Mujer' = {TM})
              #...............................
              if(Tipo_seguro=='Entera'){
                if(Seleccion_frac=='No'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, i = Tipo_interes, m = Diferido , k=1)
                  l$prima_inventario  = l$prima_pura*(1+Gastos_internos)
                  l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
                }
                if(Seleccion_frac=='Si'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, i = Tipo_interes, m = Diferido, k=1)
                  l$prima_fraccionada =  (l$prima_pura/axn(tabla, x=Edad, n = Fraccion , i=Tipo_interes, m = Diferido, k=Temporalidad ,payment = "due"))/Temporalidad
                  l$prima_inventario = l$prima_fraccionada*(1+Gastos_internos)
                  l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
                }
              }
              if(Tipo_seguro=='Temporal'){
                if(Seleccion_frac=='No'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, n = Duracion, i = Tipo_interes, m = Diferido, k=1)
                  l$prima_inventario  = l$prima_pura*(1+Gastos_internos)
                  l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
                  l$prima_nivelada = l$prima_pura/axn(tabla, x=Edad, n = Duracion, i = Tipo_interes, m = Diferido, k = 1, payment = "due")
                  l$reserva = c()
                  for(t in 0:Duracion){
                    l$reserva[t+1] = #paste0("Anio : ", t, " Reserva Matematica : ", 
                      Reserva(producto=producto,t=t,l = l,tabla=tabla,Cuantia=Cuantia,
                              Edad=Edad,Duracion=Duracion,
                              Tipo_interes=Tipo_interes,Diferido=Diferido)
                    #)
                  }
                  # plot(seq(0,Duracion), l$reserva, type='l', col=3, xlab='Anio', ylab='Reserva')
                  
                }
                if(Seleccion_frac=='Si'){
                  l$prima_pura = Cuantia*Axn(tabla, x = Edad, n= Duracion, i = Tipo_interes, m= Diferido, k=1)
                  l$prima_fraccionada =  (l$prima_pura/axn(tabla, x=Edad, n = Fraccion , i=Tipo_interes, m=Diferido, k=Temporalidad ,payment = "due"))/Temporalidad
                  l$prima_inventario = l$prima_fraccionada *(1+Gastos_internos)
                  l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
                  
                }
              }
              return(l)
            },
            #...............................
            #...............................
            '1_cuantia_variable' = { 
              l = list()
              tabla = switch(Sexo,'Hombre' = {TH},'Mujer' = {TM})
              #...............................
              if(Tipo_crecimiento=='Geometrico'){
                # cat("***********",Crecimiento)
                Interes = (Tipo_interes-Crecimiento)/(1+Crecimiento) 
                l$prima_pura = (Cuantia/(1+Crecimiento))*Axn(tabla, x = Edad,n=Duracion, i = Interes, k=1)
                l$prima_inventario = l$prima_pura*(1+Gastos_internos)
                l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
                
              }
              if(Tipo_crecimiento=='Aritmetico'){
                
                Prima1 = (Cuantia-Crecimiento)*Axn(tabla, x = Edad,n=Duracion, i = Tipo_interes, k=1)
                Prima2 = Crecimiento*Iaxn(tabla, x = Edad, i = Tipo_interes)
                cat("** Prima1=",Prima1)
                cat("** Prima2=",Prima2)
                l$prima_pura = (Prima1+Prima2)/Crecimiento # OOJO verficar formula, se divide para el crecimiento?
                l$prima_inventario  = l$prima_pura*(1+Gastos_internos)
                l$prima_comercial  = l$prima_inventario*(1+Gastos_externos)
              }
              
              return(l)
            },
            #...............................#...............................
            # Rentas   .....................#...............................
            #...............................#...............................
            '2_prepagables' = list(producto = producto,
                                   # Tipo_seguro = Tipo_seguro,
                                   Edad = Edad, Sexo=Sexo,
                                   Tipo_interes = Tipo_interes,
                                   Gastos_internos = Gastos_internos,
                                   Gastos_externos = Gastos_externos,
                                   Cuantia = Cuantia,
                                   Duracion = Duracion,
                                   Seleccion_frac = Seleccion_frac,
                                   Temporalidad = Temporalidad,
                                   Fraccion = Fraccion,
                                   Tipo_crecimiento = Tipo_crecimiento,
                                   Crecimiento = Crecimiento),
            #...............................
            #...............................
            '2_pospagables' = list(producto = producto,
                                   # Tipo_seguro = Tipo_seguro,
                                   Edad = Edad, Sexo=Sexo,
                                   Tipo_interes = Tipo_interes,
                                   Gastos_internos = Gastos_internos,
                                   Gastos_externos = Gastos_externos,
                                   Cuantia = Cuantia,
                                   Duracion = Duracion,
                                   Seleccion_frac = Seleccion_frac,
                                   Temporalidad = Temporalidad,
                                   Fraccion = Fraccion,
                                   Tipo_crecimiento = Tipo_crecimiento,
                                   Crecimiento = Crecimiento)
    )
  
  return(respuesta)
}