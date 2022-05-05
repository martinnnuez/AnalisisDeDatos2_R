setwd("C:/Users/marti/Desktop/ProyectoAnalisisDatos2/semana1/Actividades")
library(haven)
library(lavaan)

datos<- readxl::read_xls("Base_fam.xls")

#Modelo de Medida (I) con todo

# Solo conflicto
cfa_conflicto <-'
  conflicto =~ C20 + C21 + C22 + C25 + C32 + C34

  #Varianza de los Factores
  conflicto~~conflicto

  #Varianzas y covarianzas de los errores
  C20~~C20
  C21~~C21
  C22~~C22
  C25~~C25
  C32~~C32
  C34~~C34
'

fit <- lavaan(cfa_conflicto, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))

# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# conflicto =~
#   C20               1.295    0.127   10.209    0.000    1.295    0.860
# C21               1.329    0.117   11.343    0.000    1.329    0.924
# C22               1.148    0.106   10.820    0.000    1.148    0.790
# C25               1.186    0.124    9.535    0.000    1.186    0.749
# C32               1.130    0.142    7.959    0.000    1.130    0.727
# C34               1.117    0.115    9.695    0.000    1.117    0.738

# Todas las cargas son mayores a 0.7 entonces dejamos todas las variables.
# Las metricas generales para el modelo dan bien.

# CFA procesop
cfa_procesop <-'
  procesop =~ 1 * C24 + C38

  #Varianza de los Factores
  procesop~~procesop

  #Varianzas y covarianzas de los errores
  C24~~C24
  C38~~C38
'
# Me daba error, lo corro abajo sacandole una a ver si se elimina la redundancia y tiene inversa.
# Con una variable menos no lo corre tampoco, tira el warning.

# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# procesop =~
#   C24               1.000                               1.000    0.631
# C38               1.581    0.326    4.843    0.000    1.581    0.857

# Optamos por sacar la 24.

cfa_procesop <-'
  procesop =~ 1 *  C38

  #Varianza de los Factores
  procesop~~procesop

  #Varianzas y covarianzas de los errores
  C38~~C38
'

# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# procesop =~
#   C38               1.000                               1.000    0.542

# Sacando la 24 da peor, entonces las dejamos.

fit <- lavaan(cfa_procesop, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))
# Este es el que no esta estimando

# Solo dos variables, da warning lo dejamos asi.

# Solo concenso
cfa_concenso <-'
  concenso =~ C27 + C28 + C29 + C30

  #Varianza de los Factores
  concenso~~concenso

  #Varianzas y covarianzas de los errores
  C27~~C27
  C28~~C28
  C29~~C29
  C30~~C30
'
fit <- lavaan(cfa_concenso, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))

# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# concenso =~
# C27               1.040    0.109    9.554    0.000    1.040    0.672
# C28               1.011    0.101   10.000    0.000    1.011    0.732
# C29               1.299    0.085   15.293    0.000    1.299    0.945
# C30               1.213    0.099   12.195    0.000    1.213    0.811

cfa_concenso <-'
  concenso =~ C28 + C29 + C30

  #Varianza de los Factores
  concenso~~concenso

  #Varianzas y covarianzas de los errores
  C28~~C28
  C29~~C29
  C30~~C30
'
fit <- lavaan(cfa_concenso, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))

# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# concenso =~
#   C28               0.972    0.108    8.964    0.000    0.972    0.704
# C29               1.358    0.088   15.481    0.000    1.358    0.989
# C30               1.168    0.109   10.747    0.000    1.168    0.780

# Concluimos que sacamos C27 ahora da mejor.



# COMPLETO!!!

# Completo luego de ver que dejo y que no y porque
cfa_fit <-'
  conflicto =~ 1 * C20 + C21 + C22 + C25 + C32 + C34
  procesop =~ 1 * C24 + C38
  concenso =~ 1 * C27 + C28 + C29 + C30

  #Varianza de los Factores
  conflicto~~conflicto
  procesop~~procesop
  concenso~~concenso

  #Varianzas y covarianzas de los errores
  C20~~C20
  C21~~C21
  C22~~C22
  C25~~C25
  C32~~C32
  C34~~C34

  C24~~C24
  C38~~C38

  C27~~C27
  C28~~C28
  C29~~C29
  C30~~C30
'
# C27~~C27
# C38~~C38

fit <- lavaan(cfa_fit, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))

# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# conflicto =~
#   C20               1.299       NA                      1.299    0.860
# C21               1.333       NA                      1.333    0.925
# C22               1.153       NA                      1.153    0.791
# C25               1.184       NA                      1.184    0.755
# C32               1.129       NA                      1.129    0.727
# C34               1.117       NA                      1.117    0.738
# procesop =~
#   C24               1.237       NA                      1.237    0.779
# C38               1.283       NA                      1.283    0.695
# concenso =~
#   C28               0.972       NA                      0.972    0.704
# C29               1.358       NA                      1.358    0.989
# C30               1.168       NA                      1.168    0.780

# Dan joya, salvo la 38 pero como esta dentro de los limites, la dejamos.
#Grafico para solo ver las Cargas
library (semPlot)
semPaths(fit, what = "path", whatLabels = "std", style = "lisrel", rotation = 2, layout = "tree2", mar = c(1, 2, 1, 2), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 6, edge.label.cex=1.5, residuals = TRUE)


# Analizamos correlacion entre las variables
library(ggplot2)
library(ggcorrplot)
resid(fit, "cor")
ggcorrplot(residuals(fit, type="cor")$cov, type="lower")
