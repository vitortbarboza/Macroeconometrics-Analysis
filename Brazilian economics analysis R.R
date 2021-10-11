rm(list=ls()) 

library(readxl)
library(fBasics)
library(MTS)
library(vars)
library(fUnitRoots)
library(urca)
library(tseries)
library(ggplot2)
library(dplyr)
library(astsa)
library(forecast)
library(mltools)

setwd("C:/Users/vitor/Documents/Mestrado Econometria/3º Semestre/Macroeconometria II/Trabalho")
base <- read_excel("DADOS VAR.xlsx")
base$LPIB <- log(base$`PIB (Milhões de US$)`)
base$LTAXA_de_Cambio <- log(base$`Taxa de Cambio  Real`)
base <- base[,-1]
base <- base[,-2]
base <- base %>% select("LPIB", "Balança Comercial", "LTAXA_de_Cambio")

colnames(base) <- c("LPIB","Balança_Comercial","LTAXA_de_Cambio")

base <- ts(base ,start=c(1995,1),end=c(2019,12),frequency=12)
base <- window(base,end=c(2013,12))
base <- read_excel(file.choose())

par(mfrow=c(2,1))
plot(base[,5], main="Exportação em Milhoes US$ - 1995 a 2013", ylab="", xlab="")
plot(base[,6], main="Importação em Milhoes US$ - 1995 a 2013", ylab="", xlab="")

plot(base, type = "l", main="Série Dados Econômicos do Brasil - 1995 a 2013") #obtain time series plots
plot(dbase, type = "l", main="Série Diferenciada Dados Econômicos do Brasil - 1995 a 2013") #obtain time series plots
plot(log(base[,1]), type="l", main="log pib", ylab="")
plot(base[,1], type="l", main="pib original", ylab="")
#teste de raiz unitária serie original
adf.test(base[,1], k=5)
adf.test(base[,2], k=5)
adf.test(base[,3], k=5)

#diferenciação da série
dbase <- diff(base)

#teste de raiz unitaria serie transformada
adf.test(dbase[,1], k=5)
adf.test(dbase[,2], k=5)
adf.test(dbase[,3], k=5)

# ACF E PACF

acf(base[,1], main = "ACF PIB", xlab="")
acf(base[,2], main = "ACF Balança Comercial", xlab="")
acf(base[,3], main = "ACF Taxa de Cambio", xlab="")

pacf(base[,1], main = "PACF PIB", xlab="")
pacf(base[,2], main = "PACF Balança Comercial", xlab="")
pacf(base[,3], main = "PACF Taxa de Cambio", xlab="")

acf(dbase[,1], main = "ACF Dif. LPIB", xlab="")
acf(dbase[,2], main = "ACF Dif. Balança Comercial", xlab="")
acf(dbase[,3], main = "ACF Dif. LTaxa de Cambio", xlab="")

pacf(dbase[,1], main = "PACF Dif. LPIB", xlab="")
pacf(dbase[,2], main = "PACF Dif. Balança Comercial", xlab="")
pacf(dbase[,3], main = "PACF Dif. LTaxa de Cambio", xlab="")

acf(base)
acf(dbase)
pacf(dbase)
pacf(base)

# further features of the sample: basic descriptive statistics
fBasics::basicStats(base)
fBasics::basicStats(dbase)


# A fitted VAR(2) model
VAR_2<-vars::VAR(dbase, p = 2, type = "const", season = 12)
summary(VAR_2,digits=4)
y <- summary(VAR_2,digits=4)
vars::Acoef(VAR_2)
x <- vars::Bcoef(VAR_2)
coef(VAR_2)
fitted_VAR_2 <- ts(fitted(VAR_2),start = 1995, frequency = 12)
res_VAR_2 <- ts(residuals(VAR_2),start = 1995, frequency = 12)
plot(VAR_2)
comp <- round(x, 4)
comp

v <- y$varresult
coef_pib <- v$LPIB
round(u$coefficients, 4)
round(v$LPIB, 2)

coef_balança <- y$varresult
coef_balança <- coef_balança$Balança_Comercial
coef_balança$

plot(fitted_VAR_2, type = "l")
plot(res_VAR_2, type = "l")
autoplot(cbind(fitted_VAR_2[,1],dbase[,1]))
?autoplot

# A fitted VAR(1) model
VAR_1<-vars::VAR(dbase, p = 1, type = "const", season = 12)
summary(VAR_1,digits=3)
BIC(VAR_1)
vars::Acoef(VAR_1)
vars::Bcoef(VAR_1)
coef(VAR_1)
fitted_VAR_1 <- ts(fitted(VAR_1),start = 1995, frequency = 12)
res_VAR_1 <- ts(residuals(VAR_1),start = 1995, frequency = 12)
plot(VAR_1)

# A fitted VAR(2+p) model
VAR_3<-vars::VAR(dbase, p = 3, type = "const", season=12)
VAR_4<-vars::VAR(dbase, p = 4, type = "const", season=12)
VAR_5<-vars::VAR(dbase, p = 5, type = "const", season=12)
VAR_6<-vars::VAR(dbase, p = 6, type = "const", season=12)
VAR_7<-vars::VAR(dbase, p = 7, type = "const", season=12)
VAR_8<-vars::VAR(dbase, p = 8, type = "const", season=12)


## Diagnostic Testing
## ARCH test
plot(VAR_2)
## serial correlation test
for (h in 3:10){
  #h# 
  show(h)
  show(serial.test(VAR_2,lags.pt = h, type =  "PT.adjusted"))
}

?serial.test
serial.test(var_2,lags.pt = 2, type =  "PT.adjusted")

for (h in 3:10){
  #h# 
  show(h)
  show(archtest <- arch.test(VAR_2, lags.multi = h))
}

## Normality test
normalitytest <- normality.test(var_2)
plot(normalitytest)

fBasics::basicStats(resid(var_2), ci = 0.95)

plot(serialtest)
var.2c.stabil <- stability(var_2)
plot(var.2c.stabil)



## serial correlation test
for (h in 2:10){
  #h# 
  show(h)
  show(serial.test(VAR_1,lags.pt = h, type =  "PT.adjusted"))
}


serial.test(VAR_4,lags.pt = 2, type =  "PT.adjusted")

for (h in 13:20){
  #h# 
  show(h)
  show(archtest <- arch.test(VAR_12, lags.multi = h))
}


#Critérios de seleção

VARselect(dbase,lag.max = 8, type="const", season = 12)
AIC(VAR_1)
AIC(VAR_2)
AIC(VAR_3)
AIC(VAR_4)
AIC(VAR_5)
AIC(VAR_6)
AIC(VAR_7)
AIC(VAR_8)

BIC(VAR_1)
BIC(VAR_2)
BIC(VAR_3)
BIC(VAR_4)
BIC(VAR_5)
BIC(VAR_6)
BIC(VAR_7)
BIC(VAR_8)



#Granger Causality and Instantaneous Causality tests

causality(VAR_2, cause = c("LPIB","LTAXA_de_Cambio"))

causality(VAR_2, cause = c("Balança_Comercial","LTAXA_de_Cambio"))

causality(VAR_2, cause = c("LPIB","Balança_Comercial"))


#use a robust HC variance-covariance matrix for the Granger test:

causality(VAR_2, cause = c("LPIB","LTAXA_de_Cambio"), vcov.=vcovHC(VAR_2))

causality(VAR_2, cause = c("Balança_Comercial","LTAXA_de_Cambio"), vcov.=vcovHC(VAR_2))

causality(VAR_2, cause = c("LPIB","Balança_Comercial"), vcov.=vcovHC(VAR_2))
?causality


#Impulse response functions
#non orthogonal
plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "Balança_Comercial", n.ahead = 8, ortho = FALSE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95)) 

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "Balança_Comercial", n.ahead = 8, ortho = FALSE, 
               cumulative = TRUE, boot = TRUE, ci = 0.95)) 


plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = FALSE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95)) 

plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = FALSE, 
               cumulative = TRUE, boot = TRUE, ci = 0.95)) 

#Orthogonal: recommended
##normal
par(mfrow=c(3,3))

plot(vars::irf(VAR_2, impulse = "LPIB", response = "LPIB", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95)) 

plot(vars::irf(VAR_2, impulse = "LPIB", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LPIB", response = "Balança_Comercial", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))


plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "Balança_Comercial", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "LPIB", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "LPIB", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "Balança_Comercial", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95))

##Acumulado

plot(vars::irf(VAR_2, impulse = "LPIB", response = "LPIB", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95)) 

plot(vars::irf(VAR_2, impulse = "LPIB", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LPIB", response = "Balança_Comercial", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))


plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "Balança_Comercial", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "LPIB", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "Balança_Comercial", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "LTAXA_de_Cambio", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "LPIB", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))

plot(vars::irf(VAR_2, impulse = "LTAXA_de_Cambio", response = "Balança_Comercial", n.ahead = 8, ortho = TRUE, 
               cumulative = T, boot = TRUE, ci = 0.95))




#Look at the restrictions on the contemponaneous effects
#imposed by the Cholesky decomposition

plot(vars::irf(VAR_2, impulse = "cons", response = "invest", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95)) 

plot(vars::irf(VAR_2, impulse = "income", response = "invest", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95)) 

plot(vars::irf(VAR_2, impulse = "cons", response = "income", n.ahead = 8, ortho = TRUE, 
               cumulative = FALSE, boot = TRUE, ci = 0.95)) 

?irf
#Forecast Error Variance Decomposition

vars::fevd(VAR_2,n.ahead = 8)




#####Previsão
#modelo sarima

lpib <- base[,1]
lpib_treino <- lpib[1:204]
lpib_treino <- ts(lpib_treino, start=c(1995,1),end=c(2011,12),frequency=12)
lpib_teste <- lpib[205:228]
lpib_teste <- ts(lpib_teste, start=c(2012,1),end=c(2013,12),frequency=12)
dlpib_treino <- diff(lpib_treino)

plot(lpib_treino)
adf.test(diff(diff(lpib_treino),12))
plot(diff(lpib_treino), main="Série LPIB de 1995 a 2011", ylab="")


par(mfrow=c(2,1))
acf(diff(lpib_treino), 50)
pacf(diff(lpib_treino), 50)

plot(diff(diff(lpib_treino),12))
acf(diff(diff(lpib),12), 50, main="ACF DIF. LPIB LAG 1 e 12")
pacf(diff(diff(lpib_treino),12), 50, main="PACF DIF. LPIB LAG 1 e 12")



auto.arima(lpib_treino, d=1, D=1, max.p = 1, max.q = 1, max.P =1 , max.Q =1)
sarima(lpib_treino,0,1,0,0,1,1,12)
modelo <- sarima(lpib_treino,0,1,0,0,1,1,12)
modelo

modelo$ttable
?sarima.for
previsao <- sarima.for(lpib_treino, 24,0,1,0,0,1,1,12)
previsao
previsao$pred
lpib_teste
mse(previsao$pred, lpib_teste)*100


#####Previsão 2
#modelo sarima

lpib <- base[,1]
dlpib <- diff(lpib)
dlpib_treino <- dlpib[1:203]
dlpib_treino<- ts(dlpib_treino, start=c(1995,2),end=c(2011,12), frequency = 12)
dlpib_teste <- dlpib[204:227]
dlpib_teste <- ts(dlpib_teste, start=c(2012,1),end=c(2013,12),frequency=12)




plot(lpib_treino)
adf.test(diff(diff(lpib_treino),12))
plot(diff(lpib_treino), main="Série LPIB de 1995 a 2011", ylab="")


par(mfrow=c(2,1))
acf(diff(lpib_treino), 50)
pacf(diff(lpib_treino), 50)

plot(diff(diff(lpib_treino),12))
acf(diff(diff(lpib_treino),12), 50)
pacf(diff(diff(lpib_treino),12), 50)



auto.arima(dlpib_treino, d=0, D=1, max.p = 1, max.q = 1, max.P =1 , max.Q =1)
sarima(dlpib_treino,0,0,0,0,1,1,12)
modelo <- sarima(dlpib_treino,0,0,0,0,1,1,12)
modelo

modelo$ttable
?sarima.for
previsao <- sarima.for(dlpib_treino, 24,0,0,0,0,1,1,12)
previsao
previsao$pred
dlpib_teste
mse(previsao$pred, dlpib_teste)*100



#Forecasting with fitted VAR models
dbase_treino <- dbase[1:203,]
dbase_treino<- ts(dbase_treino, start=c(1995,2),end=c(2011,12), frequency = 12)
dbase_teste <- dbase[204:227,]
dbase_teste <- ts(dbase_teste, start=c(2012,1),end=c(2013,12),frequency=12)

VAR_previsao<-vars::VAR(dbase_treino, p = 2, type = "const", season = 12)



VAR_2_prd <- predict(VAR_previsao, n.ahead = 24, ci = 0.95)
VAR_2_prd$fcst
x<- VAR_2_prd$fcst
xx <- x$LPIB
xx
y <- ts(xx[,1], start=c(2012,1),end=c(2013,12),frequency=12)
?predict
plot(VAR_2_prd)
fanchart(VAR_2_prd)
y
mse(y, dbase_teste[,1])*100
dbase_teste[,1]
mse(VAR_2_prd$fcst[,1], dbase_teste[,1])*100
VAR_2_prd$fcst[,2]

x$LPIB



#Forecast
#Forecasting with fitted VAR models
VAR_2_prd <- predict(VAR_2, n.ahead = 24, ci = 0.95)
plot(VAR_2_prd)
fanchart(VAR_2_prd)
VAR_2_prd