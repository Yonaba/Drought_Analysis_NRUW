setwd("D:/Recherche/Indices_Drought/R")

#Chargement des libraries
library(biwavelet)
library(fields)

library(WaveletComp)
library(ggplot2)
library(readxl)
#Chargement des données
Sys.setenv(TZ = "UTC")
X<-read_excel("data/Indices1.xlsx",
              sheet = '1mois')

#Creation des matrices (séries et valeurs)
t1 <- cbind(1:510, X$Y)
t2 <- cbind(1:510, X$X)

dev.off()
# Compute Cross-wavelet
xwt.t1t2 <- xwt(t1, t2)
par(oma=c(0,0,0,1), mar=c(5,4,5,5)+0.4)
plot(xwt.t1t2, xlab="Year", ylab="Period (Months)",
     font.lab=2, plot.cb = F, plot.phase = TRUE, 
     xaxt = 'n', type="power", plot.coi = TRUE, lty.coi = 1, col.coi = "black", lwd.coi = 3,
     plot.sig=TRUE, lwd.sig=2, arrow.lwd=0.04, arrow.len=0.10,
     main = "A) XWT:SPI.12-SSI.12")
min.pow <- min(xwt.t1t2$power)
max.pow <- max(xwt.t1t2$power)
image.plot( zlim=c(min.pow,max.pow), nlevel = 64,legend.only=TRUE)
# insertion des dates de la période d'étude
n=length(t1[, 1])
axis(side = 1, at=c(seq(0, n, 11.6)), labels=c(seq(1971, 2014, 1)))

