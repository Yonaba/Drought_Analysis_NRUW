setwd("D:/Recherche/Indices_Drought/R")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(ggcorrplot)
library(patchwork)

corr_method <- "spearman"

make_plot <- function(m1, m2, corr_method, pl_title) {
  n <- as.numeric(ncol(m1))
  cmat <- pmat <- matrix(nrow=n, ncol=n)  
  for (x in 1:n) {
    for (y in 1:n) {
      ctest <- cor.test(m1[,x], m2[,n-y+1], alternative = "two.sided", method = corr_method, 
                        conf.level = 0.95, exact = T, continuity = T)
      cmat[x,y] <- ctest$estimate
      pmat[x,y] <- ctest$p.value
    }
  }
  rownames(cmat) <- rownames(pmat) <- colnames(m1)
  colnames(cmat) <- colnames(pmat) <- rev(colnames(m2))  
  pl <- ggcorrplot(cmat, 
                   legend.title = paste0("Correlation"),
                   title = pl_title,
                   outline.color = "black",
                   lab = T, lab_col = "black", insig = "blank",
                   p.mat = pmat, sig.level = 0.05, digits = 3,
                   ggtheme = theme_bw(),
                   colors = c("white", "lightgreen", "red")) + 
    #ylim=c(0,1) +
    theme(plot.title = element_text(face = "bold"))
  return (pl)
}

data <- read.csv("tables/spi_spei_ssi_wayenF.csv", header = T, sep = ",", dec = ".")

spi <- data[,2:6]
spei <- data[,7:11]
ssi <- data[,12:16]

pl1 <- make_plot(spi, ssi, corr_method, "a) SPI - SSI")
pl2 <- make_plot(spei, ssi, corr_method, "b) SPEI - SSI")

patch <- pl1 + pl2 + plot_layout(guides = "collect")

ggsave(filename = paste0("graphs/corr_spi_spei_ssi_",corr_method,".png"),patch,units = "cm",
       width = 10, height = 6, dpi = 400,scale = 2.8, limitsize = FALSE)

print("done.")
