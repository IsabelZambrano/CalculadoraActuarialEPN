library(lifecontingencies)
library(readxl)

# Probabilidades de muerte
probH <- unname(unlist(read_excel("datos/Tabla_Mortalidad_Ecuador_Ejercicios.xlsx", sheet = 1)[,c(4)]))
probM <- unname(unlist(read_excel("datos/Tabla_Mortalidad_Ecuador_Ejercicios.xlsx", sheet = 3)[,c(4)]))

# Tabla de mortalidad
TH <- probs2lifetable(probH, radix = 1000000, type = "qx", name = "Ecuador")
TM <- probs2lifetable(probM, radix = 1000000, type = "qx", name = "Ecuador")

# Exportar
save(TH,TM,file='datos/tablas_listas.RData')
