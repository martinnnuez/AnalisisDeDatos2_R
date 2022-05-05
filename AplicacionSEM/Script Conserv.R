#cONSERVADURISMO

#Modelo de medida

#Importamos la base de datos desde SPSS/EXCEL
library(haven)
Ej_Conservador <- read_dta("C:/Users/usuario/Dropbox/backup/para PC/Mis documentos/2016/Curso SEM/Curso 2016/Bases/Ej_Conservador.dta")
View(Ej_Conservador)

#Renombrar la base
datos<-Ej_Conservador


#Sintaxis AFC----
library(lavaan)

#Modelo de Medida (I)
CONSERV.CFA <-'
CONS=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10


#Varianza de los Factores
CONS~~CONS


#Covarianzas de los Factores


#Varianzas y covarianzas de los errores
x1~~x1
x2~~x2
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x8~~x8
x9~~x9
x10~~x10

'


fit <- lavaan(CONSERV.CFA, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))

# Modelo de medida 2 sacamos x10 por tener menor carga

CONSERV.CFA <-'
CONS=~x1+x2+x3+x4+x5+x6+x7+x8+x9


#Varianza de los Factores
#Se fijan a 1, con el comando std.lv=TRUE (o bien, SC~~1*SC)
CONS~~CONS


#Covarianzas de los Factores


#Varianzas y covarianzas de los errores
x1~~x1
x2~~x2
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x8~~x8
x9~~x9


'


fit <- lavaan(CONSERV.CFA, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr"))

#Grafico para solo ver las Cargas
library (semPlot)
semPaths(fit, what = "path", whatLabels = "std", style = "lisrel", rotation = 2, layout = "tree2", mar = c(1, 2, 1, 2), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 6, edge.label.cex=1.5, residuals = TRUE)

#Analisis de Residuos
library(ggcorrplot)
resid(fit, "cor")
ggcorrplot(residuals(fit, type="cor")$cov, type="lower")

#Analisis de Fiabilidad y Validez----
library (semTools)
reliability(fit)
htmt(CONSERV.CFA, datos)

#Reespecificaciones

modindices(fit, sort.=TRUE, minimum.value = 3.84)

# Modelo de medida 3 sin x10 por tener menor carga y sin x2 y x8 por marco teorico

CONSERV.CFA <-'
CONS=~x1+x3+x4+x5+x6+x7+x9


#Varianza de los Factores
CONS~~CONS


#Covarianzas de los Factores


#Varianzas y covarianzas de los errores
x1~~x1
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x9~~x9


'


fit <- lavaan(CONSERV.CFA, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr"))

#Grafico para solo ver las Cargas
library (semPlot)
semPaths(fit, what = "path", whatLabels = "std", style = "lisrel", rotation = 2, layout = "tree2", mar = c(1, 2, 1, 2), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 6, edge.label.cex=1.5, residuals = TRUE)

#Analisis de Residuos
library(ggcorrplot)
resid(fit, "cor")
ggcorrplot(residuals(fit, type="cor")$cov, type="lower")

#Analisis de Fiabilidad y Validez----
library (semTools)
reliability(fit)


#Reespecificaciones

modindices(fit, sort.=TRUE, minimum.value = 3.84)

# Modelo de medida 4 sin x10 por tener menor carga, sin x2 y x8 por marco teorico y agregando la corr entre los errores de x3 y x4

CONSERV.CFA <-'
CONS=~x1+x3+x4+x5+x6+x7+x9



#Varianza de los Factores
CONS~~CONS


#Covarianzas de los Factores


#Varianzas  de los errores

x1~~x1
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x9~~x9


# Covarianzas de los errores
x3~~x4

'


fit <- lavaan(CONSERV.CFA, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr"))



#Modelo de Medida (4)
DEP.CFA <-'
DEP=~x11+x12+x13


#Varianza de los Factores
DEP~~DEP


#Covarianzas de los Factores


#Varianzas y covarianzas de los errores

x11~~x11
x12~~x12
x13~~x13

'


fit <- lavaan(DEP.CFA, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr"))



# mODELO DE MEDIDA CON DOS FACTORES
CONSDEP.CFA <-'
CONS=~x1+x3+x4+x5+x6+x7+x9
DEP=~x11+x12+x13


#Varianza de los Factores
CONS~~CONS
DEP~~DEP

#Covarianzas de los Factores
CONS~~DEP

#Varianzas  de los errores

x1~~x1
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x9~~x9

x11~~x11
x12~~x12
x13~~x13

# Covarianzas de los errores
x3~~x4

'


fit <- lavaan(CONSDEP.CFA, data=datos, std.lv=TRUE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr"))


# Modelo estructural

CONSDEP2.CFA <-'
CONS=~1*x1+x3+x4+x5+x6+x7+x9
DEP=~1*x11+x12+x13

#Modelo de relaciones
CONS~DEP


#Varianza de los Factores independientes
DEP~~DEP

#Varianza de los Factores independientes
CONS~~CONS


#Varianzas y covarianzas de los errores

x1~~x1
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x9~~x9

x11~~x11
x12~~x12
x13~~x13

# Covarianzas de los errores
x3~~x4

'


fit <- lavaan(CONSDEP2.CFA, data=datos, std.lv=FALSE, mimic="eqs", estimator="MLM", verbose=TRUE, warn=TRUE)

#Elementos de Salida----
summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


fitMeasures (fit, c("cfi.robust", "nnfi.robust", "ifi.scaled", "tli.robust", "rmsea.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr"))


#Grafico para solo ver las Cargas
library (semPlot)
semPaths(fit, what = "path", whatLabels = "std", style = "lisrel", rotation = 2, layout = "tree2", mar = c(1, 2, 1, 2), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 6, edge.label.cex=1.5, residuals = TRUE)


??semPaths
