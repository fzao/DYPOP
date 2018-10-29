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

fig3_export <- function(ids, FD, t_10, t_90, cache, XL=40, Dso=NULL)
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
    
    dataF <- data.frame(x, d0l, d0m, d0h,
                        d1l, d1m, d1h,
                        dAdl, dAdm, dAdh)

    return(dataF)
}
