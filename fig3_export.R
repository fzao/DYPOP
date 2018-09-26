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

    dataF <- data.frame(x, d0l, d0m, d0h,
                        d1l, d1m, d1h,
                        dAdl, dAdm, dAdh)

    return(dataF)
}
