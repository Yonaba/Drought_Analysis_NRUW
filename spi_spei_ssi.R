setwd("D:/Recherche/Indices_Drought/R")
Sys.setenv(TZ = "UTC")

#Chargement des libraries
library(zoo)
library(stringr)
library(SPEI)
library(ggplot2)
library(patchwork)

#chargement des données à la station
station <- "wayenF"
data <- read.csv(paste0("data/",station,".csv"), header = T, sep = ",", dec = ".")

#remplissage des valeurs manquantes
pr <- na.approx(data$pr)
bal <- (pr - data$pet)
lr <- na.approx(data$lr)

#conversion en séries temporelles
pr.ts <- ts(data=pr, start=c(data$year[1], data$month[1]), frequency = 12)
bal.ts <- ts(data=bal, start=c(data$year[1], data$month[1]), frequency = 12)
lr.ts <- ts(data=lr, start=c(data$year[1], data$month[1]), frequency = 12)

#creation de la série de dates
n <- as.numeric(nrow(data))
s_date <- as.Date(paste0(data$year[1],"-",data$month[1],"-01"))
e_date <- as.Date(paste0(data$year[n],"-",data$month[n],"-01"))
dates <- seq(s_date, e_date, by="month")

#fonction pour plotter un indice standardisé
#avec coloration automatiques des valeurs en dessous de zéro en rouge
#et les valeurs en dessus de zéro en bleu
plot.si <- function (si,dates, period, si.lab, station, i) {
  df <- data.frame(dates, si)
  colnames(df) <- c("dates", "si")
  pl <- ggplot(data=df, aes(x=dates, y=si)) + 
    geom_ribbon(aes(ymin=pmin(si,0), ymax=0), fill="red", col="red", alpha=0.5) +
    geom_ribbon(aes(ymin=0, ymax=pmax(si,0)), fill="blue", col="blue", alpha=0.5) +
    ylim(-4,4) + ylab(si.lab) + xlab("") + 
    labs(title = paste0(letters[i],") ",si.lab,"-",period)) +
    theme_bw()+
    theme(plot.title=element_text(size=18, face="bold"))
  
  return (pl)
}

#fonction pour plotter les séries temporelles (pluie, PET et écoulement)
plot.clivar <- function(df,var, lims,color, ylabel, pl.title, station) {
  pl <- ggplot(data=df, aes(x=dates, y=.data[[var]])) + 
    geom_line(color=color) + xlab("") + ylab(ylabel) +
    ylim(lims) +
    labs(title=paste0(pl.title," / ",str_to_title(station))) +
    theme_bw()
  return (pl)
}

#liste des pas de temps de période  
periods <- c(1,3,6,12,24)
#periods <- c(1)
i <- 0
pl <- list()

spi.out <- spei.out <- ssi.out <- data.frame(matrix(nrow=nrow(data), ncol=0))

#calcul des série des SPI, SPEI et SSI pour chaque période
for (period in periods) {
  #period <- 1
  print(period)
  ddates <- dates #dates[seq(1,n,period)]
  
  spi.d <- spi(pr.ts,period, distribution = "Gamma")
  spi.v <- as.numeric(spi.d$fitted)
  spi.v[!is.finite(spi.v)] <- NA
  #spi.vv <- spi.v[seq(1,n,period)]
  spi.out <- data.frame(spi.out, spi.v)
  
  spei.d <- spei(bal.ts,period,distribution = "log-Logistic")
  spei.v <- as.numeric(spei.d$fitted)
  spei.v[!is.finite(spei.v)] <- NA
  #spei.vv <- spei.v[seq(1,n,period)]
  spei.out <- data.frame(spei.out, spei.v)
  
  ssi.d <- spi(lr.ts,period, distribution = "Gamma")
  ssi.v <- as.numeric(ssi.d$fitted)
  ssi.v[!is.finite(ssi.v)] <- NA
  #ssi.vv <- ssi.v[seq(1,n,period)]
  ssi.out <- data.frame(ssi.out, ssi.v)
  
  pl[[length(pl)+1]] <- plot.si(spi.v, ddates, period, "SPI", station, i+1)
  pl[[length(pl)+1]] <- plot.si(spei.v, ddates, period, "SPEI", station, i+2)
  pl[[length(pl)+1]] <- plot.si(ssi.v, ddates, period, "SSI", station, i+3)
  i <- i+3
  
  
  #spei.d <- spei(bal.ts,period)
  #ssi.d <- spi(lr.ts,period)
  
  #plot(spi.d, main=paste0("SPI-",period," / ",str_to_title(station)), )
}

colnames(spi.out) <- paste0("SPI-",periods)
colnames(spei.out) <- paste0("SPEI-",periods)
colnames(ssi.out) <- paste0("SSI-",periods)

# préparation du csv en sortie, en filtrant les valeurs d'indice selon les
# pas de temps de période
out.df <- data.frame(dates,spi.out, spei.out, ssi.out)

# fdf <- data.frame(dates)
# cnames <- colnames(out.df)[-1]
# pp <- rep(periods,3)
# for (i in 1:length(cnames)) {
#   cname <- cnames[i]
#   si <- rep(NA,n)
#   si[seq(0,n,pp[i])] <- out.df[seq(0,n,pp[i]),cname]
#   fdf <- data.frame(fdf,si)
# }
#colnames(fdf) <- colnames(out.df)
write.csv(out.df, file=paste0("tables/spi_spei_ssi_",station,".csv"), row.names=F)

# exportation du graphique des indices standardisés
grob <- wrap_plots(pl, ncol=3, guides = "collect")
ggsave(filename = paste0("graphs/spi_spei_ssi_",station,".png"), grob, dpi = 400, 
       scale = 1.5, width = 25, height = 20, unit = "cm")

# exportation du graphique des séries temporelles (pluie, PET et écoulement)
cli.station <- data.frame(dates, pr, data$pet, lr)
colnames(cli.station) <- c("dates", "pr", "pet", "lr")

pr.pl <- plot.clivar(cli.station,"pr", c(0,300),"blue", "Rainfall [mm]","a) Monthly Rainfall", station)
pet.pl <- plot.clivar(cli.station,"pet", c(0,250),"red", "PET [mm]","b) Monthly PET", station)
lr.pl <- plot.clivar(cli.station,"lr", c(0,50),"maroon", "Surface runoff [mm]","c) Monthly Surface Runoff", station)
grob.cli <- wrap_plots(pr.pl, pet.pl, lr.pl, ncol=1, guides = "collect")

ggsave(filename = paste0("graphs/",station,"_cli.png"), grob.cli, dpi = 400, 
       scale = 1.5, width = 10, height = 15, unit = "cm")

print("done.")
