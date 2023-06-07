setwd("D:/Recherche/Indices_Drought/R")
Sys.setenv(TZ = "UTC")

#Chargement des libraries
library(e1071)
library(PearsonDS)
library(zoo)
library(lubridate)
library(actuar)
library(fitdistrplus)
library(ggplot2)

#Chargement des données de la station
station <- "wayenF"
data <- read.csv(paste0("data/",station,".csv"), header = T, sep = ",", dec = ".")

#remplissage des valeurs manquantes
pr <- na.approx(data$pr)            #pluies
bal <- (na.approx(pr - data$pet))   #P-ETP
lr <- na.approx(data$lr)            #Ecoulements

# Construction de la serie de dates
n <- as.numeric(nrow(data))
s_date <- as.Date(paste0(data$year[1],"-",data$month[1],"-01"))
e_date <- as.Date(paste0(data$year[n],"-",data$month[n],"-01"))
dates <- seq(s_date, e_date, by="month")

df <- data.frame(dates,month(dates),pr,bal,lr)
colnames(df)[2] <- "month"

# Matrice de donnees en sortie
df.out <- data.frame(matrix(nrow=0,ncol=5))
colnames(df.out) <- c("var","dis","period","mon","KS-p.value")

periods <- c(1,3,6,12,24)                 #periodes d'analyse
vars <- c("pr", "bal","lr")               #variables à analyser
var.dis <- c("gamma","llogis","gamma")    #distribution à tester
names(var.dis) <- vars

for (var in vars) {
  #var <- "lr"
  print(paste0("Processing ",var))
  ddf <- data.frame(df$month, df[,var])
  colnames(ddf) <- c("month", var)
  
  for (period in periods) {
    #period <- 1
    print(paste0("  Period ",period))
    for (mon in 1:12) {
      #mon <- 6
      print(paste0("      month ",mon))
      
      #creation de la serie de données accumulée, sans 0 et NAs
      ss <- ddf[ddf$month==mon,var]
      ss <- ss [!ss %in% c(0,NA)]
      ss <- rollsum(ss, k=period, align = "right", fill = NA)
      ss <- ss [!ss %in% c(0,NA)]
      
      #Dans le cas de la variable "bal", la distribution est shiftée
      #pour ajuster une loi log-logistique à 2 paramètres
      if (var == "bal") ss <- (ss - min(ss))+1 
      
      cc <- c(var,as.character(var.dis[var]),period, mon,"-")
      if (length(ss)>1) {
        
        #Ajustement de la distribution
        fit <- fitdist(ss,distr = as.character(var.dis[var]), method="mle")
        shape <- scale <- rate <- location <- NULL
        
        shape <- fit$estimate[[1]]
        rate <- fit$estimate[[2]]
        scale <- rate
        if (var.dis[var] != "llogis") scale <- 1/rate
          
        #Test de Komogorov Smirnov
        ks <- NULL
        if (var.dis[var] != "lnorm") {
          ks <- ks.test(jitter(ss),
                        paste0("p",fit$distname),
                        shape = shape, scale = scale, 
                        alternative = "two.sided", exact=T)
        } else {
          ks <- ks.test(jitter(ss),
                        paste0("p",fit$distname),
                        shape, rate, 
                        alternative = "two.sided", exact=T)          
        }
        cc[5] <- ks$p.value
      } 
      df.out[nrow(df.out)+1,] <- cc
    }
  }
}

#View(df.out)
#Ecriture du CSV en sortie
write.csv(df.out, file=paste0("tables/check_dis",station,"ks-test.csv"), row.names=F)

# Production du graphique
dd <- df.out
dd$dis <- dd$mon <- NULL
dd$var <- as.factor(dd$var)
dd$period <- as.factor(as.numeric(dd$period))
colnames(dd)[3] <- "pvalue"
dd$pvalue <- as.numeric(dd$pvalue)

fnames <- list(
  'pr'="a) P [mm] (Gamma dist.)",
  'bal'="b) P - PET [mm] (Log-Logistic dist.)",
  'lr'= "c) Q [mm] (Gamma dist.)"
)

f_labeller <- function(variable,value){
  return(fnames[value])
}

pl <- ggplot(dd, aes(x=period, y=pvalue)) + 
  geom_boxplot() + ylab("K-S test p-value") + xlab("Periods [months]") +
  facet_grid(~factor(var,levels=vars), labeller = f_labeller) +
  geom_hline(yintercept = 0.05, linetype = "dashed", col= "red", linewidth = 0.5) + 
  theme_bw() +
  annotate("text",x=2,y = 0.05, label = "Sig. Level 0.05", vjust = 1.5, 
           size = 3, fontface = "bold.italic", color = "red")

pl

#Ecriture du graphique en sortie
ggsave(filename = paste0("graphs/ks_test_",station,".png"), pl, dpi = 400, width = 10, height = 6, unit = "cm",scale=2)
