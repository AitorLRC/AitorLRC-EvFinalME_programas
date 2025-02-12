library(RcmdrMisc)

####################
#Lectura del fichero
####################
setwd("C:/Users/aitor/Desktop/EvFinalME/AitorLRC-EvFinalME_programas/")

load("Auto.RData")

Auto1 <- Auto

head(Auto1)

summary(Auto1)

###############
#Pasa la variable recorrido a kilometros por litro
###############

Auto1$mpg <- Auto1$mpg *(1.60934/3.78541)
colnames(Auto1)[11] <- 'kmpl'


attach(Auto1)

n.puertas.factor <- as.factor(n.puertas)
compresion.factor <- as.factor(compresion)


###############
#Determina que variables est�n relacionadas con el consumo y describe esa relaci�n
###############

#Empezamos calculando la matriz de correlacion entre las variables
correlationM <- cor(Auto1[, c('long', 'ancho', 'alto', 'peso', 'cilindrada', 'potencia', 'rpm', 'kmpl')])
correlationM[, 'kmpl']
#Salvo alto y rpm todas tienen una correlaci�n alta lo que indica a que est�n relacionadas con el consumo

reg.1 <- glm(kmpl~1)

reg.puertas <- glm(kmpl~n.puertas.factor); reg.puertas
anova(reg.1,reg.puertas,test="Chisq") #0.7 por lo que el n�mero de puertas no es significativo, no est� relacionado

reg.long <- glm(kmpl~long); summary(reg.long)
anova(reg.long,test="Chisq") #menor por lo que la longitud s� est� relacionada

reg.ancho <- glm(kmpl~ancho); summary(reg.ancho)
anova(reg.ancho,test="Chisq") #menor, s� est� relacionado

reg.alto <- glm(kmpl~alto); summary(reg.alto)
anova(reg.alto,test="Chisq") #Mayor, no est� relacionado

reg.peso <- glm(kmpl~peso); summary(reg.peso)
anova(reg.peso,test="Chisq") # menor, s� relacionada

reg.cilindrada <- glm(kmpl~cilindrada); summary(reg.cilindrada)
anova(reg.cilindrada,test="Chisq") # menor, s� relacionada

reg.compresion <- glm(kmpl~compresion.factor); reg.compresion
summary(reg.compresion)
anova(reg.compresion,test="Chisq") #p menor que 0.05 por lo que compresion s� es significativo

reg.potencia <- glm(kmpl~potencia); summary(reg.potencia)
anova(reg.potencia,test="Chisq") #menor, s� relacionada

reg.rpm <- glm(kmpl~rpm); summary(reg.rpm)
anova(reg.rpm,test="Chisq") #mayor, no relacionada


#RESUMEN POR GRUPOS

numSummary(kmpl, groups = n.puertas.factor)
Boxplot(kmpl~n.puertas.factor, data=Auto1, id=list(method="y"))

numSummary(kmpl, groups =compresion.factor )
Boxplot(kmpl~compresion.factor, data=Auto1, id=list(method="y"))


###############
#Construye un modelo que permita predecir el recorrido
###############

#Construyo un modelo con todas las variables explicativas
reg.model.tot <- glm(kmpl ~ n.puertas.factor + long + ancho + peso + cilindrada + compresion.factor + potencia)
summary(reg.model.tot)

reg.model <- stepwise(reg.model.tot)
summary(reg.model)


###############
#Calcula un intervalo de confianza
###############

confint(reg.model)

pos <- which(long == 450 &
               ancho == 168 &
               peso == 1137 &
               cilindrada == 2229 &
               compresion.factor == 'media' &
               potencia == 110)

pred <- predict(reg.model)

hi <- pred+1.96*sd(pred)
lo <- pred-1.96*sd(pred)

pred2 <- pred[pos]; pred2
hi2 <- hi[pos]; hi2
lo2 <- lo[pos]; lo2


