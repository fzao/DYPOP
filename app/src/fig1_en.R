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
fig1_en <- function(ids, FS, t_10, t_90, cache, val0=0.1)
{
    par_name <- paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    disc <- length(FS[["X0"]])

    xAd <- c(val0, FS[["XAd"]][c(2:disc)])
    sAd_025 <- FS[[par_name]][,'rAd_025']/xAd
    sAd_25 <- FS[[par_name]][,'rAd_25']/xAd
    sAd_50 <- FS[[par_name]][,'rAd_50']/xAd
    sAd_75 <- FS[[par_name]][,'rAd_75']/xAd
    sAd_975 <- FS[[par_name]][,'rAd_975']/xAd

    # bad values and smoothing
    threshold <- 1.e-8
    sAd_025[is.na(sAd_025)] <- 0.
    sAd_025[sAd_025<threshold] <- 0.
    smoothing <- loess(sAd_025 ~ xAd)
    sAd_025 <- smoothing$fitted
    sAd_25[is.na(sAd_25)] <- 0.
    sAd_25[sAd_25<threshold] <- 0.
    smoothing <- loess(sAd_25 ~ xAd)
    sAd_25 <- smoothing$fitted
    sAd_50[is.na(sAd_50)] <- 0.
    sAd_50[sAd_50<threshold] <- 0.
    smoothing <- loess(sAd_50 ~ xAd)
    sAd_50 <- smoothing$fitted
    sAd_75[is.na(sAd_75)] <- 0.
    sAd_75[sAd_75<threshold] <- 0.
    smoothing <- loess(sAd_75 ~ xAd)
    sAd_75 <- smoothing$fitted
    sAd_975[is.na(sAd_975)] <- 0.
    sAd_975[sAd_975<threshold] <- 0.
    smoothing <- loess(sAd_975 ~ xAd)
    sAd_975 <- smoothing$fitted

    dataF <- data.frame(xAd, sAd_025,sAd_25,sAd_50,sAd_75,sAd_975)

    p <- plot_ly(dataF, x = ~xAd, y = ~sAd_975, type = 'scatter', mode = 'lines',
            line = list(color = 'black'),
            showlegend = FALSE, name = 'Percentile 97.5') %>%
      add_trace(y = ~ sAd_025, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'black'),
                showlegend = FALSE, name = 'Percentile 2.5') %>%
      add_trace(y = ~ sAd_25, type = 'scatter', mode = 'lines',
                line = list(color='black'),
                showlegend = FALSE, name = 'Percentile 25') %>%
      add_trace(y = ~ sAd_75, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(50,50,50,0.2)', line = list(color = 'black'),
                showlegend = FALSE, name = 'Percentile 75') %>%
      add_trace(y = ~ sAd_50, type = 'scatter', mode = 'lines',
                line = list(color='red'),
                showlegend = FALSE, name = 'Median') %>%
      layout(#title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             title = 'Annual apparent survival of adult trout (> 1)',
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = ">1+ density the previous year",
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
