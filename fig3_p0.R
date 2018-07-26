#########################################
###  FIGURE 3 : SURVIVAL RATE
### Global prediction for the station
### (what range of survival to expect based on
### %Caches and T10/T50
#########################################
# ids : name of the station (used for the title)
# FD : Data loaded (corresponding to the input parameter set)
# XL = max length of abscissa
# Dso : Data to plot

fig3_p0 <- function(ids, FD, t_10, t_90, cache, XL=40, Dso=NULL)
{
    par_name <- paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
    SEQstep <- FD[['xinf']][2]-FD[['xinf']][1]

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


    name_l <- "Low recruitment"
    name_m <- "Medium recruitment"
    name_h <- "High recruitment"
    p <- plot_ly(dataF, x = ~x, y = ~d0l, type = 'scatter', mode = 'none', name = name_l, fill = 'tozeroy',
            fillcolor = 'rgba(50, 185, 50, 0.5)') %>%
      add_trace(x = ~x, y = ~d0m, name = name_m, fill = 'tozeroy',
                fillcolor = 'rgba(50, 50, 185, 0.5)') %>%
      add_trace(x = ~x, y = ~d0h, name = name_h, fill = 'tozeroy',
                fillcolor = 'rgba(185, 50, 50, 0.5)') %>%
      layout(xaxis = list(title = 'D[0+,y]'),
             yaxis = list(title = 'Density')) %>%
      layout(legend = list(x = 0.9, y = 0.9))

 
    return(p)
    }
