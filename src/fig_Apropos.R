#
#  _______     _______   ____  _____
# |  __ \ \   / /  __ \ / __ \|  __ \
# | |  | \ \_/ /| |__) | |  | | |__) |
# | |  | |\   / |  ___/| |  | |  ___/
# | |__| | | |  | |    | |__| | |
# |_____/  |_|  |_|     \____/|_|
#
# Un outil d'aide au diagnostic de l'etat des populations de truite fario
#
# Copyright (c) EDF-IRSTEA 2018
#
# Auteurs : Fabrice Zaoui - Victor Bret
#
# Licence CeCILL v2.1
#
#
#########################################
### Fonction de visualisation des donnees de peches utilisees
####
#########################################

figAP_hist_p0 <- function(binwidth=2)
{
  D <- read.table("data/DATA TRF_AllY.csv", sep=";", dec=',', header=TRUE)
  p <- plot_ly(D, x = ~N100m2_0, type = "histogram", nbinsx=ceiling(max(D$N100m2_0, na.rm=TRUE)/binwidth), hoverinfo="x+y") %>%
        layout(xaxis = list(title = 'Densites de 0+ (N/100m2)'),
           yaxis = list(title = 'Nombre'))

	return(p)
	}


figAP_hist_p1 <- function(binwidth=2)
{
  D <- read.table("data/DATA TRF_AllY.csv", sep=";", dec=',', header=TRUE)
  p <- plot_ly(D, x = ~N100m2_1, type = "histogram", nbinsx=ceiling(max(D$N100m2_1, na.rm=TRUE)/binwidth), hoverinfo="x+y") %>%
    layout(xaxis = list(title = 'Densites de 1+ (N/100m2)'),
           yaxis = list(title = 'Nombre'))

  return(p)
}



figAP_hist_pAd <- function(binwidth=2)
{
  D <- read.table("data/DATA TRF_AllY.csv", sep=";", dec=',', header=TRUE)
  p <- plot_ly(D, x = ~N100m2_Ad, type = "histogram", nbinsx=ceiling(max(D$N100m2_Ad, na.rm=TRUE)/binwidth), hoverinfo="x+y") %>%
    layout(xaxis = list(title = 'Densites de Ad (N/100m2)'),
           yaxis = list(title = 'Nombre'))

  return(p)
}
