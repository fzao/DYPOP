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
fig1_p0 <- function(ids, FS, t_10, t_90, cache, val0=0.1)
{
    par_name <- paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    disc <- length(FS[["X0"]])
	
    x0 <- c(val0, FS[["X0"]][c(2:disc)]) # x0 for survival estimates (cannot be 0)
    s0_025 <- FS[[par_name]][,'r1_025']/x0
    s0_25 <- FS[[par_name]][,'r1_25']/x0
    s0_50 <- FS[[par_name]][,'r1_50']/x0
    s0_75 <- FS[[par_name]][,'r1_75']/x0
    s0_975 <- FS[[par_name]][,'r1_975']/x0

    # bad values and smoothing
    threshold <- 1.e-8
    s0_025[is.na(s0_025)] <- 0.
    s0_025[s0_025<threshold] <- 0.
    smoothing <- loess(s0_025 ~ x0)
    s0_025 <- smoothing$fitted
    s0_25[is.na(s0_25)] <- 0.
    s0_25[s0_25<threshold] <- 0.
    smoothing <- loess(s0_25 ~ x0)
    s0_25 <- smoothing$fitted
    s0_50[is.na(s0_50)] <- 0.
    s0_50[s0_50<threshold] <- 0.
    smoothing <- loess(s0_50 ~ x0)
    s0_50 <- smoothing$fitted
    s0_75[is.na(s0_75)] <- 0.
    s0_75[s0_75<threshold] <- 0.
    smoothing <- loess(s0_75 ~ x0)
    s0_75 <- smoothing$fitted
    s0_975[is.na(s0_975)] <- 0.
    s0_975[s0_975<threshold] <- 0.
    smoothing <- loess(s0_975 ~ x0)
    s0_975 <- smoothing$fitted

    dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975)
    
    p <- plot_ly(dataF, x = ~x0, y = ~s0_975, type = 'scatter', mode = 'lines',
            line = list(color = 'black'),
            showlegend = FALSE, name = 'Percentile 97.5') %>%
      add_trace(y = ~ s0_025, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'black'),
                showlegend = FALSE, name = 'Percentile 2.5') %>%
      add_trace(y = ~ s0_25, type = 'scatter', mode = 'lines',
                line = list(color='black'),
                showlegend = FALSE, name = 'Percentile 25') %>%
      add_trace(y = ~ s0_75, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(50,50,50,0.2)', line = list(color = 'black'),
                showlegend = FALSE, name = 'Percentile 75') %>%
      add_trace(y = ~ s0_50, type = 'scatter', mode = 'lines',
                line = list(color='red'),
                showlegend = FALSE, name = 'Median') %>%
      layout(title = "Annual apparent survival of the young of the year (0+)",
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = "Density of 0+ the previous year",
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
