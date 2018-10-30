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

    # bad values
    d0l[is.na(d0l)] <- 0.
    d0l[d0l<1.e-6] <- 0.
    d0m[is.na(d0m)] <- 0.
    d0m[d0m<1.e-6] <- 0.
    d0h[is.na(d0h)] <- 0.
    d0h[d0h<1.e-6] <- 0.
    d1l[is.na(d1l)] <- 0.
    d1l[d1l<1.e-6] <- 0.
    d1m[is.na(d1m)] <- 0.
    d1m[d1m<1.e-6] <- 0.
    d1h[is.na(d1h)] <- 0.
    d1h[d1h<1.e-6] <- 0.
    dAdl[is.na(dAdl)] <- 0.
    dAdl[dAdl<1.e-6] <- 0.
    dAdm[is.na(dAdm)] <- 0.
    dAdm[dAdm<1.e-6] <- 0.
    dAdh[is.na(dAdh)] <- 0.
    dAdh[dAdh<1.e-6] <- 0.
    
    # threshold at xmax = 100
    x <- x[x<101.]
    lenx <- length(x)
    d0l <- d0l[1:lenx]
    d0m <- d0m[1:lenx]
    d0h <- d0h[1:lenx]
    d1l <- d1l[1:lenx]
    d1m <- d1m[1:lenx]
    d1h <- d1h[1:lenx]
    dAdl <- dAdl[1:lenx]
    dAdm <- dAdm[1:lenx]
    dAdh <- dAdh[1:lenx]
    
    # spline smoothing
    cstep <- 0.2
    dstep <- x[2] - x[1]
    nstep <- cstep * dstep
    xs <- seq(x[1], x[lenx], nstep)
    smoothing <- splinefun(x, d0l)
    d0l <- smoothing(xs)
    smoothing <- splinefun(x, d0m)
    d0m <- smoothing(xs)
    smoothing <- splinefun(x, d0h)
    d0h <- smoothing(xs)
    smoothing <- splinefun(x, d1l)
    d1l <- smoothing(xs)
    smoothing <- splinefun(x, d1m)
    d1m <- smoothing(xs)
    smoothing <- splinefun(x, d1h)
    d1h <- smoothing(xs)
    smoothing <- splinefun(x, dAdl)
    dAdl <- smoothing(xs)
    smoothing <- splinefun(x, dAdm)
    dAdm <- smoothing(xs)
    smoothing <- splinefun(x, dAdh)
    dAdh <- smoothing(xs)
    x <- xs
    
    dataF <- data.frame(x, d0l, d0m, d0h,
                        d1l, d1m, d1h,
                        dAdl, dAdm, dAdh)


    name_l <- "Recrutement faible"
    name_m <- "Recrutement moyen"
    name_h <- "Recrutement fort"
    p <- plot_ly(dataF, x = ~x, y = ~d0l, type = 'scatter', mode = 'none', name = name_l, fill = 'tozeroy',
            fillcolor = 'rgba(50, 185, 50, 0.5)') %>%
      add_trace(x = ~x, y = ~d0m, name = name_m, fill = 'tozeroy',
                fillcolor = 'rgba(50, 50, 185, 0.5)') %>%
      add_trace(x = ~x, y = ~d0h, name = name_h, fill = 'tozeroy',
                fillcolor = 'rgba(185, 50, 50, 0.5)') %>%
      layout(xaxis = list(title = "Densité de 0+ l'année (n)"),
             yaxis = list(title = 'Densité')) %>%
      layout(legend = list(x = 0.9, y = 0.9))

 
    return(p)
    }
