

# Texto Informacion y Guia de Usuario  ...................................
TextoInformacion = function(producto){
  
  texto = switch (producto,
                  '1_fallecimiento' = {
                    tags$span(
                      tags$div(align='left',
                               tags$p('La aseguradora garantiza el pago de un capital en caso de ocurrir el fallecimiento del asegurado dentro del periodo de vigencia del contrato.'),
                               tags$p(tags$b('Temporal:'),'Se garantiza la indemnización o prestación sólo si el asegurado fallece dentro de los n años de vigencia del contrato.'),
                               tags$p(tags$b('Vida entera:'),' Se garantiza la indemnización o prestación con independencia del momento de fallecimiento del asegurado.'),
                               tags$ul(
                                 tags$li(tags$b('Gastos internos:'),' Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Gastos externos:'),' Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Cobertura:'),' Periodo de vigencia en años del contrato.'),
                                 tags$li(tags$b('Fraccionar la prima:'),' Cuando el pago de la prima no se realiza en un solo desembolso y se consideran pagos de frecuencia inferior a un año.'),
                                 tags$li(tags$b('Periodicidad de pagos:'),' Número de periodos en los cuales se ha fraccionado el año.'),
                                 tags$li(tags$b('Plazo de pago (años):'),' Número de años en los que se va a pagar la prima.'),
                                 tags$li(tags$b('Gastos externos:'),' Representan los gastos de comercialización y mantenimiento de clientes.')
                                 
                               )
                      ))
                    
                  },
                  '1_supervivencia' = {
                    tags$span(
                      tags$div(align='left',
                               tags$p('La aseguradora garantiza el pago de un capital si el asegurado sobrevive un determinado periodo definido en el contrato.'),
                               tags$ul(
                                 tags$li(tags$b('Gastos internos:'),' Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Gastos externos:'),'  Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Cobertura:'),'  Periodo de vigencia en años del contrato.'),
                                 tags$li(tags$b('Fraccionar la prima:'),'  Cuando el pago de la prima no se realiza en un solo desembolso y se consideran pagos de frecuencia inferior a un año.'),
                                 tags$li(tags$b('Periodicidad de pagos:'),'  Número de periodos en los cuales se ha fraccionado el año.'),
                                 tags$li(tags$b('Plazo de pago (años): '),' Número de años en los que se va a pagar la prima.')
                               )
                      ))
                  },
                  '1_mixto' = {
                    tags$span(
                      tags$div(align='left',
                               tags$p('Se garantiza el pago de la prestación o indemnización en el caso de que el asegurado 
                                                fallezca dentro de un determinado periodo de tiempo o también si el asegurado sobrevive
                                                al finalizar la vigencia de dicho contrato.'),
                               tags$ul(
                                 tags$li(tags$b('Gastos internos:'),' Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Gastos externos:'),'  Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Cobertura:'),'  Periodo de vigencia en años del contrato.'),
                                 tags$li(tags$b('Fraccionar la prima:'),'  Cuando el pago de la prima no se realiza en un solo desembolso y se consideran pagos de frecuencia inferior a un año.'),
                                 tags$li(tags$b('Periodicidad de pagos:'),'  Número de periodos en los cuales se ha fraccionado el año.'),
                                 tags$li(tags$b('Plazo de pago (años):'),'  Número de años en los que se va a pagar la prima.')
                               )
                      ))
                  },
                  '1_diferido' = {
                    tags$span(
                      tags$div(align='left',
                               tags$p('Provee la prestación o indemnización al beneficiario de un seguro solo si el 
                                                asegurado sobrevive al menos m años después de establecida la póliza.'),
                               tags$ul(
                                 tags$li(tags$b('Gastos internos:'), 'Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Gastos externos:'), ' Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Cobertura:'), ' Periodo de vigencia en años del contrato.'),
                                 tags$li(tags$b('Fraccionar la prima:'), ' Cuando el pago de la prima no se realiza en un solo desembolso y se consideran pagos de frecuencia inferior a un año.'),
                                 tags$li(tags$b('Periodicidad de pagos:'), ' Número de periodos en los cuales se ha fraccionado el año.'),
                                 tags$li(tags$b('Plazo de pago (años):'), ' Número de años en los que se va a pagar la prima.'),
                                 tags$li(tags$b('Diferimiento:'), ' La cantidad de años que se va a diferir el seguro de vida.')
                               )
                      ))
                  },
                  '1_cuantia_variable'={
                    tags$span(
                      tags$div(align='left',
                               tags$p(
                                 'Seguros de vida en los que la cuantía varía con el tiempo siguiendo una progresión aritmética o geométrica.'),
                               tags$ul(
                                 tags$li(tags$b('Gastos internos:'),' Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Gastos externos:'),'  Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Cobertura:'),'  Periodo de vigencia en años del contrato.'),
                                 tags$li(tags$b('Tipo de crecimiento:'),'  Puede ser aritmético o geométrico.'),
                                 tags$li(tags$b('Crecimiento:'),'  Valor o porcentaje en los que variará la cuantía cada año.')
                               )
                      ))
                  },
                  '2_prepagables'={
                    tags$span(
                      tags$div(align='left',
                               tags$p('Este tipo de renta actuarial consiste en el pago de una cuantía constante al inicio de cada uno de los años.'),
                               tags$p(tags$b('Vitalicia:'),' Se pagará el valor de la renta durante los siguientes años hasta el fallecimiento del asegurado.'),
                               tags$p(tags$b('Temporal:'),' Se pagará el valor de la renta durante n años, mientras sobreviva el asegurado.'),
                               tags$ul(
                                 tags$li(tags$b('Gastos internos:'),' Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Gastos externos:'),'  Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li(tags$b('Cobertura:'),'  Periodo de vigencia en años del contrato.'),
                                 tags$li(tags$b('Fraccionar la prima:'),'  Cuando el pago de la prima no se realiza en un solo desembolso y se consideran pagos de frecuencia inferior a un año.'),
                                 tags$li(tags$b('Periodicidad de pagos:'),'  Número de periodos en los cuales se ha fraccionado el año.'),
                                 tags$li(tags$b('Plazo de pago (años):'),'  Número de años en los que se va a pagar la prima.')
                               )
                      ))
                  },
                  '2_pospagables'={
                    tags$span(
                      tags$div(align='left',
                               tags$p(
                                 'Este tipo de renta actuarial consiste en el pago de una cuantía constante al final de cada uno de los años.'),
                               tags$p(tags$b('Vitalicia:'),' Se pagará el valor de la renta durante los siguientes años hasta el fallecimiento del asegurado.'),
                               tags$p(tags$b('Temporal:'),' Se pagará el valor de la renta durante n años, mientras sobreviva el asegurado.'),
                               tags$ul(
                                 tags$li('Gastos internos: Representan los gastos en los que incurre la compañía exceptuando los de comercialización y mantenimiento de clientes.'),
                                 tags$li('Gastos externos: Representan los gastos de comercialización y mantenimiento de clientes.'),
                                 tags$li('Cobertura: Periodo de vigencia en años del contrato.'),
                                 tags$li('Fraccionar la prima: Cuando el pago de la prima no se realiza en un solo desembolso y se consideran pagos de frecuencia inferior a un año.'),
                                 tags$li('Periodicidad de pagos: Número de periodos en los cuales se ha fraccionado el año.'),
                                 tags$li('Plazo de pago (años): Número de años en los que se va a pagar la prima.')
                               )
                      ))
                    
                  }
  )
  return(texto)
  
}

TextoTitulo = function(producto){
  texto = switch(producto,
                 '1_fallecimiento' = {"Seguros de Vida en Caso de Fallecimiento"},
                 '1_supervivencia' = {"Seguros de Vida en Caso de Supervivencia"},
                 '1_mixto' = {"Seguros de Vida Mixtos"},
                 '1_diferido' = {"Seguros de Vida Diferidos"},
                 '1_cuantia_variable' = {"Seguros de Cuantía Variable"},
                 '2_prepagables' = {"Rentas Actuariales Prepagables"},
                 '2_pospagables' = {"Rentas Actuariales Pospagables"},
  )
  return(texto)
}
