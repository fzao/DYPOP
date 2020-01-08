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
###  FIGURE 1 : SURVIVAL RATE
### Global prediction for the station
### (what range of survival to expect based on
### %Caches and T10/T50
#########################################
# ids : name of the station (used for the title)
# FS : Data loaded (corresponding to the input parameter set)
# val0 : value taken for computation for x=0 (cannot be)
fig1_p1 <- function(ids, FS, t_10, t_90, cache, val0=0.1)
{
    par_name <- paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    disc <- length(FS[["X0"]])
	
    x1 <- c(val0, FS[["X1"]][c(2:disc)])
    s1_025 <- FS[[par_name]][,'r2_025']/x1
    s1_25 <- FS[[par_name]][,'r2_25']/x1
    s1_50 <- FS[[par_name]][,'r2_50']/x1
    s1_75 <- FS[[par_name]][,'r2_75']/x1
    s1_975 <- FS[[par_name]][,'r2_975']/x1

    # bad values and smoothing
    threshold <- 1.e-8
    s1_025[is.na(s1_025)] <- 0.
    s1_025[s1_025<threshold] <- 0.
    smoothing <- loess(s1_025 ~ x1)
    s1_025 <- smoothing$fitted
    s1_25[is.na(s1_25)] <- 0.
    s1_25[s1_25<threshold] <- 0.
    smoothing <- loess(s1_25 ~ x1)
    s1_25 <- smoothing$fitted
    s1_50[is.na(s1_50)] <- 0.
    s1_50[s1_50<threshold] <- 0.
    smoothing <- loess(s1_50 ~ x1)
    s1_50 <- smoothing$fitted
    s1_75[is.na(s1_75)] <- 0.
    s1_75[s1_75<threshold] <- 0.
    smoothing <- loess(s1_75 ~ x1)
    s1_75 <- smoothing$fitted
    s1_975[is.na(s1_975)] <- 0.
    s1_975[s1_975<threshold] <- 0.
    smoothing <- loess(s1_975 ~ x1)
    s1_975 <- smoothing$fitted
    
    dataF <- data.frame(x1, s1_025,s1_25,s1_50,s1_75,s1_975)

    p <- plot_ly(dataF, x = ~x1, y = ~s1_975, type = 'scatter', mode = 'lines',
            line = list(color = 'black'),
            showlegend = FALSE, name = 'Percentile 97.5') %>%
      add_trace(y = ~ s1_025, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'black'),
                showlegend = FALSE, name = 'Percentile 2.5') %>%
      add_trace(y = ~ s1_25, type = 'scatter', mode = 'lines',
                line = list(color='black'),
                showlegend = FALSE, name = 'Percentile 25') %>%
      add_trace(y = ~ s1_75, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(50,50,50,0.2)', line = list(color = 'black'),
                showlegend = FALSE, name = 'Percentile 75') %>%
      add_trace(y = ~ s1_50, type = 'scatter', mode = 'lines',
                line = list(color='red'),
                showlegend = FALSE, name = 'Median') %>%
      layout(#title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             title = "Annual apparent survival of 1 year trout (1+)",
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = "Density of 1+ the previous year",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "Survival rate",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE,
                          range=c(0,1)))

    return(p)
}
