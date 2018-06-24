###########################################
### FIGURE 3 : 'SR' type relation: density
### of a year-class year y according to
### the densities of the age class
### preceding the year y-1
### Integration of a density level 1+
### or Ad influencing survival
#########################################
# ids : name of the station (used for the title)
# FD : Data loaded (corresponding to the input parameter set)
# t_10, t_90, cache : modelling parameter values (percentiles and % of cache)
# X1m, XAdm : Abscissa range
# Dso : Data to plot

fig3 <- function(ids, FS, t_10, t_90, cache, X1m, XAdm, Dso=NULL)
{
    par_name <- paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    par_name_full <- paste('T10',t_10,'T90',t_90,'C',cache,'X1m',X1m,'XAdm',XAdm, sep="_")
    disc <- length(FS[["X0"]])

    # Data to plot
    if (!is.null(Dso)){
    ### We put in shape to be able to quickly call on successive years
    Dso[,c('D0y-1','D1y-1','DAdy-1')]<-NA
    for (i in 1:nrow(Dso)){
        yi <- as.numeric(Dso[i,'Year'])
        if(nrow(Dso[Dso$Year==(yi-1),])==1){
            Dso[Dso$Year==(yi),c('D0y-1','D1y-1','DAdy-1')] <- Dso[Dso$Year==(yi-1),c('D0','D1','DAd')]
            }
        }
    }

    # Formatting Data
    x0 <- FS[["X0"]]
    s0_025 <- FS[[par_name]][,'r1_025']
    s0_25 <- FS[[par_name]][,'r1_25']
    s0_50 <- FS[[par_name]][,'r1_50']
    s0_75 <- FS[[par_name]][,'r1_75']
    s0_975 <- FS[[par_name]][,'r1_975']

    x1 <- FS[["X1"]]
    s1_025 <- FS[[par_name_full]][,'r2_025_Adm']
    s1_25 <- FS[[par_name_full]][,'r2_25_Adm']
    s1_50 <- FS[[par_name_full]][,'r2_50_Adm']
    s1_75 <- FS[[par_name_full]][,'r2_75_Adm']
    s1_975 <- FS[[par_name_full]][,'r2_975_Adm']

    xAd <- FS[["XAd"]]
    sAd_025 <- FS[[par_name_full]][,'rAd_025_1m']
    sAd_25 <- FS[[par_name_full]][,'rAd_25_1m']
    sAd_50 <- FS[[par_name_full]][,'rAd_50_1m']
    sAd_75 <- FS[[par_name_full]][,'rAd_75_1m']
    sAd_975 <- FS[[par_name_full]][,'rAd_975_1m']

    dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975,
        x1, s1_025,s1_25,s1_50,s1_75,s1_975,
        xAd, sAd_025,sAd_25,sAd_50,sAd_75,sAd_975)

    ## Subplots
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
                showlegend = FALSE, name = 'Percentile 25') %>%
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
             yaxis = list(title = "D[1+, y]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))

    ## Available Data?
    if (!is.null(Dso)){
    p0 <- p0 %>% add_markers(x = c(10, 30, 25, 50), y = c(1, 2.5, 1.7, 0.9), mode='markers', showlegend = FALSE)
        }

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
                showlegend = FALSE, name = 'Percentile 25') %>%
      layout(title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = paste("D[1+, y-1] (D[>1+, y-1]=",XAdm,sep=""),
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "D[>1+, y]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))


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
                showlegend = FALSE, name = 'Percentile 25') %>%
      layout(title = paste('Survival : ',ids,' (Caches=',cache,'%; T10=',t_10,'°C; T90=',t_90,'°C)',sep=''),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
             xaxis = list(title = paste("D[>1+, y-1] (D[1+, y-1]=",X1m,sep=""),
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "D[>1+, y1]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
    m <- list(
      l = 50,
      r = 50,
      b = 50,
      t = 100,
      pad = 2
    )

    p <- subplot(p0, p1, pAd, nrows=3, margin=0.05, titleX=TRUE, titleY=TRUE) %>%
      layout(autosize = T, width = 1200, height = 1000, margin = m)

    return(p)
}
