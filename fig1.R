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
	
    x0 <- c(val0, FS[["X0"]][c(2:disc)]) # x0 for survival estimates (cannot be 0)
    s0_025 <- FS[[par_name]][,'r1_025']/x0
    s0_25 <- FS[[par_name]][,'r1_25']/x0
    s0_50 <- FS[[par_name]][,'r1_50']/x0
    s0_75 <- FS[[par_name]][,'r1_75']/x0
    s0_975 <- FS[[par_name]][,'r1_975']/x0

    
	  x1 <- c(val0, FS[["X1"]][c(2:disc)])
    s1_025 <- FS[[par_name]][,'r2_025']/x1
    s1_25 <- FS[[par_name]][,'r2_25']/x1
    s1_50 <- FS[[par_name]][,'r2_50']/x1
    s1_75 <- FS[[par_name]][,'r2_75']/x1
    s1_975 <- FS[[par_name]][,'r2_975']/x1

    xAd <- c(val0, FS[["XAd"]][c(2:disc)])
    sAd_025 <- FS[[par_name]][,'rAd_025']/xAd
    sAd_25 <- FS[[par_name]][,'rAd_25']/xAd
    sAd_50 <- FS[[par_name]][,'rAd_50']/xAd
    sAd_75 <- FS[[par_name]][,'rAd_75']/xAd
    sAd_975 <- FS[[par_name]][,'rAd_975']/xAd

    dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975,
        x1, s1_025,s1_25,s1_50,s1_75,s1_975,
        xAd, sAd_025,sAd_25,sAd_50,sAd_75,sAd_975)

    p0 <- plot_ly(dataF, x = ~x0, y = ~s0_975, type = 'scatter', mode = 'lines',
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
      layout(title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = "D[0+, y-1]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "12-months\napp. survival",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE,
                          range=c(0,1)))


    p1 <- plot_ly(dataF, x = ~x1, y = ~s1_975, type = 'scatter', mode = 'lines',
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
      layout(title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = "D[1+, y-1]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "12-months\napp. survival",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE,
                          range=c(0,1)))

    pAd <- plot_ly(dataF, x = ~xAd, y = ~sAd_975, type = 'scatter', mode = 'lines',
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
      layout(title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = "D[>1+, y-1]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "12-months\napp. survival",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE,
                          range=c(0,1)))
    m <- list(
      l = 50,
      r = 50,
      b = 50,
      t = 100,
      pad = 2
    )

    p <- subplot(p0, p1, pAd, nrows=3, margin=0.05, titleX = TRUE, titleY = TRUE) %>%
      layout(autosize = T, width = 1200, height = 1000, margin = m)

    return(p)
}
