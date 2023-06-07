setwd("D:/Recherche/Indices_Drought/R")

#Chargement des libraries
library(biwavelet)
library(ggplot2)
library(readxl)
#Chargement des données
data<-read_excel("data/Indices1.xlsx",
                 sheet = '1mois2')
#Creation des matrices (séries et valeurs)
t1<-cbind(1:510, data$X)
t2<-cbind(1:510, data$Y)
nrands=500
sum(is.na(t1)) #Check how many missing value int1
sum(is.na(t2))

#this command calculate the wavelet of t1 and t2
wtc.AB=wtc(t1,t2,nrands = nrands)
par(oma=c(0,0,0,1), mar=c(5,4,5,5)+0.1)
plot(wtc.AB, plot.phase = TRUE, xaxt = 'n', lty.coi = 1, col.coi = "black", lwd.coi = 3,
     lwd.sig=2, arrow.lwd=0.04, arrow.len=0.10, ylab="Period(Month)", xlab = "Year",font.lab=2, plot.cb = T, main="D) WTC:SPEI.12-SSI.12")
# insertion des dates de la période d'étude
n=length(t1[, 1])
axis(side = 1, at=c(seq(0, n, 11.6)), labels=c(seq(1971, 2014, 1)))

