#########################################
### Fonction de creation de la FIGURE 1 : TAUX DE SURVIE
#### Prediction globale pour la station (quelle gamme de survie à attendre en fonction des %Caches et T50?
#########################################
    # ids : nom de la station (utilisé pour le titre)
    # FD : Données chargée (correspondant au set de paramètres en entrée)
    # [temp] OS : type d'OS (adapte les commandes d'ouverture de fenetres graphique
    # XL = longueur max des abcisses
    # Dso : Données a afficher
    
fig2 <- function(ids, FD, t_10, t_90, cache, XL=40, Dso=NULL)
{    
    par_name=paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    SEQstep<-FD[['xinf']][2]-FD[['xinf']][1]
    
    
    x <- FD[['xinf']]+SEQstep/2
    d0l <- FD[['X0l']]
    d0m <- FD[['X0m']]
    d0h <- FD[['X0h']]

    d1l <- FD[[par_name]]$D1l
    d1m <- FD[[par_name]]$D1m
    d1h <- FD[[par_name]]$D1h

    dAdl <- FD[[par_name]]$DAdl
    dAdm <- FD[[par_name]]$DAdm
    dAdh <- FD[[par_name]]$DAdh
    

    dataF <- data.frame(x, d0l, d0m, d0h,
                        d1l, d1m, d1h,
                        dAdl, dAdm, dAdh)

    #### TEMP : 
    dataF <- dataF[dataF$x<60,]

    name_l="Low recruitment"
    name_m="Medium recruitment"
    name_h="High recruitment"
    p0 <- plot_ly(dataF, x = ~x, y = ~d0l, type = 'scatter', mode = 'none', name = name_l, fill = 'tozeroy',
            fillcolor = 'rgba(50, 185, 50, 0.5)') %>%
      add_trace(x = ~x, y = ~d0m, name = name_m, fill = 'tozeroy',
                fillcolor = 'rgba(50, 50, 185, 0.5)') %>%
      add_trace(x = ~x, y = ~d0h, name = name_h, fill = 'tozeroy',
                fillcolor = 'rgba(185, 50, 50, 0.5)') %>%
      layout(xaxis = list(title = 'D[0+,y]'),
             yaxis = list(title = 'Densité de probabilité'))

             
    p1 <- plot_ly(dataF, x = ~x, y = ~d1l, type = 'scatter', mode = 'none', name = name_l, fill = 'tozeroy',
            showlegend = FALSE, fillcolor = 'rgba(50, 185, 50, 0.5)') %>%
      add_trace(x = ~x, y = ~d1m, name = name_m, fill = 'tozeroy',
                showlegend = FALSE, fillcolor = 'rgba(50, 50, 185, 0.5)') %>%
      add_trace(x = ~x, y = ~d1h, name = name_h, fill = 'tozeroy',
                showlegend = FALSE, fillcolor = 'rgba(185, 50, 50, 0.5)') %>%
      layout(xaxis = list(title = 'D[1+, y+1]'),
             yaxis = list(title = 'Densité de probabilité'))
             
    pAd <- plot_ly(dataF, x = ~x, y = ~dAdl, type = 'scatter', mode = 'none', name = name_l, fill = 'tozeroy',
            showlegend = FALSE, fillcolor = 'rgba(50, 185, 50, 0.5)') %>%
      add_trace(x = ~x, y = ~dAdm, name = name_m, fill = 'tozeroy',
                showlegend = FALSE, fillcolor = 'rgba(50, 50, 185, 0.5)') %>%
      add_trace(x = ~x, y = ~dAdh, name = name_h, fill = 'tozeroy',
                showlegend = FALSE, fillcolor = 'rgba(185, 50, 50, 0.5)') %>%
      layout(xaxis = list(title = 'D[>1+, y+2]'),
             yaxis = list(title = 'Densité de probabilité'))
             
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
