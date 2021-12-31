library(RcmdrMisc)

####################
#Lectura del fichero
####################
setwd("C:/Users/aitor/Desktop/EvFinalME")

load("EV.RData")

EV1 <- EV

summary(EV)


###########
#Recodificar las variables familia y ev
###########

EV1$familia <- factor(EV$familia,
                      levels = c(0,1),
                      labels = c('no', 'si'))

EV1$ev <- factor(EV$ev, 
                 levels = c(0,1),
                 labels = c('no', 'si')) 


pos <- which(EV$edad < 60 | EV$edad > 65)
EV1$edad[pos] <- NA

pos2 <- which(EV$tabaco > 80)
EV1$tabaco[pos2] <- NA

pos3 <- which(EV$alcohol > 200)
EV1$alcohol[pos3] <- NA

summary(EV1)
attach(EV1)


###########
#Describe la relación de las variables independientes con la presencia de la enfermedad vascular
###########

#Edad
numSummary(edad, groups = ev)
Boxplot(edad~ev, data=EV1, id=list(method="y"))
#La edad no parece ser de importancia en el modelo

#Pas
numSummary(pas, groups = ev) #Los que sí tienen un pas mayor
Boxplot(pas~ev, data=EV1, id=list(method="y"))
# Pas tiene significancia pero apenas apreciable

#tabaco
numSummary(tabaco, groups = ev) #Los que sí fuman más
Boxplot(tabaco~ev, data=EV1, id=list(method="y"))
#Tabaco sí tiene significancia en el modelo

#ldl
numSummary(ldl, groups = ev) #Los que sí tienen un colesterol más alto
Boxplot(ldl~ev, data=EV1, id=list(method="y"))
#ldl sí tiene significancia en el modelo

#obesidad
numSummary(obesidad, groups = ev) #Los que sí tienen, por lo general, mayor grado de obesidad
Boxplot(obesidad~ev, data=EV1, id=list(method="y"))
#obesidad tiene significancia pero es apenas apreciable

#Alcohol
numSummary(alcohol, groups = ev) #Los que sí beben mucho más alcohol
Boxplot(alcohol~ev, data=EV1, id=list(method="y"))
#Alcohol sí tiene significancia 

#Familia
summary(familia)
#Tabla de contingencia
local({
  .Table <- xtabs(~ev+familia, data=EV1)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
with(EV1, Barplot(familia, by=ev, style="divided", legend.pos="above", xlab="familia", ylab="Frequency", label.bars=TRUE))
#Poner esta gráfica y no la tabla

###########
#Establece un modelo que permita predecir la enfermedad vascular en una persona
###########

GLM.1 <- glm(ev~pas+tabaco+ldl+obesidad+alcohol+familia, family=binomial(logit))

GLM <- stepwise(GLM.1, criterion='BIC')

summary(GLM)
coef(GLM)
exp(coef(GLM))


###########
#Estima la probabilidad de enfermedad
###########

p1 <- predict(GLM, data.frame(tabaco = 0,
                              ldl = 4.7,
                              alcohol = 7,
                              familia = 'no'),
              type="response"); p1

p2 <- predict(GLM, data.frame(tabaco = 10,
                              ldl = 7,
                              alcohol = 0.4,
                              familia = 'si'),
              type="response"); p2

p1 <- predict(GLM, data.frame(tabaco = 0,
                              ldl = 4.7,
                              alcohol = 7,
                              familia = 'no'))
