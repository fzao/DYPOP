#########################################
###  FIGURE 1 : SURVIVAL RATE
### Global prediction for the station
### (what range of survival to expect based on
### %Caches and T10/T50
#########################################
# ids : name of the station (used for the title)
# FS : Data loaded (corresponding to the input parameter set)
# val0 : value taken for computation for x=0 (cannot be)
fig1 <- function(ids, FS, t_10, t_90, cache, val0=0.1)
{
    par_name <- paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    disc <- length(FS[["X0"]])
	
    xAd <- c(val0, FS[["XAd"]][c(2:disc)])
    sAd_025 <- FS[[par_name]][,'rAd_025']/xAd
    sAd_25 <- FS[[par_name]][,'rAd_25']/xAd
    sAd_50 <- FS[[par_name]][,'rAd_50']/xAd
    sAd_75 <- FS[[par_name]][,'rAd_75']/xAd
    sAd_975 <- FS[[par_name]][,'rAd_975']/xAd

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
             title = 'Adults Survival',
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = "D[>1+, y-1]",
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
