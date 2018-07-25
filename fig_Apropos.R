#########################################
### Fonction de visualisation des donnees de peches utilisees
#### 
#########################################

figAP_hist_p0 <- function(binwidth=2)
{
  D <- read.table("data/DATA TRF_AllY.csv", sep=";", dec=',', header=TRUE)
  p <- plot_ly(D, x = ~N100m2_0, type = "histogram", nbinsx=ceiling(max(D$N100m2_0, na.rm=TRUE)/binwidth), hoverinfo="x+y") %>%
        layout(xaxis = list(title = 'Densites de 0+ (N/100m²)'),
           yaxis = list(title = 'Nombre'))
  
	return(p)
	}


figAP_hist_p1 <- function(binwidth=2)
{
  D <- read.table("data/DATA TRF_AllY.csv", sep=";", dec=',', header=TRUE)
  p <- plot_ly(D, x = ~N100m2_1, type = "histogram", nbinsx=ceiling(max(D$N100m2_1, na.rm=TRUE)/binwidth), hoverinfo="x+y") %>%
    layout(xaxis = list(title = 'Densites de 1+ (N/100m²)'),
           yaxis = list(title = 'Nombre'))
  
  return(p)
}



figAP_hist_pAd <- function(binwidth=2)
{
  D <- read.table("data/DATA TRF_AllY.csv", sep=";", dec=',', header=TRUE)
  p <- plot_ly(D, x = ~N100m2_Ad, type = "histogram", nbinsx=ceiling(max(D$N100m2_Ad, na.rm=TRUE)/binwidth), hoverinfo="x+y") %>%
    layout(xaxis = list(title = 'Densites de Ad (N/100m²)'),
           yaxis = list(title = 'Nombre'))
  
  return(p)
}





