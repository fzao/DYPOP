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

fig1_export <- function(ids, FS, t_10, t_90, cache, val0=0.1)
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
    s0_025[is.na(s0_025)] <- 0.
    s0_025[s0_025<1.e-6] <- 0.
    smoothing <- loess(s0_025 ~ x0)
    s0_025 <- smoothing$fitted
    s0_25[is.na(s0_25)] <- 0.
    s0_25[s0_25<1.e-6] <- 0.
    smoothing <- loess(s0_25 ~ x0)
    s0_25 <- smoothing$fitted
    s0_50[is.na(s0_50)] <- 0.
    s0_50[s0_50<1.e-6] <- 0.
    smoothing <- loess(s0_50 ~ x0)
    s0_50 <- smoothing$fitted
    s0_75[is.na(s0_75)] <- 0.
    s0_75[s0_75<1.e-6] <- 0.
    smoothing <- loess(s0_75 ~ x0)
    s0_75 <- smoothing$fitted
    s0_975[is.na(s0_975)] <- 0.
    s0_975[s0_975<1.e-6] <- 0.
    smoothing <- loess(s0_975 ~ x0)
    s0_975 <- smoothing$fitted

    x1 <- c(val0, FS[["X1"]][c(2:disc)])
    s1_025 <- FS[[par_name]][,'r2_025']/x1
    s1_25 <- FS[[par_name]][,'r2_25']/x1
    s1_50 <- FS[[par_name]][,'r2_50']/x1
    s1_75 <- FS[[par_name]][,'r2_75']/x1
    s1_975 <- FS[[par_name]][,'r2_975']/x1
    
    # bad values and smoothing
    s1_025[is.na(s1_025)] <- 0.
    s1_025[s1_025<1.e-6] <- 0.
    smoothing <- loess(s1_025 ~ x1)
    s1_025 <- smoothing$fitted
    s1_25[is.na(s1_25)] <- 0.
    s1_25[s1_25<1.e-6] <- 0.
    smoothing <- loess(s1_25 ~ x1)
    s1_25 <- smoothing$fitted
    s1_50[is.na(s1_50)] <- 0.
    s1_50[s1_50<1.e-6] <- 0.
    smoothing <- loess(s1_50 ~ x1)
    s1_50 <- smoothing$fitted
    s1_75[is.na(s1_75)] <- 0.
    s1_75[s1_75<1.e-6] <- 0.
    smoothing <- loess(s1_75 ~ x1)
    s1_75 <- smoothing$fitted
    s1_975[is.na(s1_975)] <- 0.
    s1_975[s1_975<1.e-6] <- 0.
    smoothing <- loess(s1_975 ~ x1)
    s1_975 <- smoothing$fitted
    
    xAd <- c(val0, FS[["XAd"]][c(2:disc)])
    sAd_025 <- FS[[par_name]][,'rAd_025']/xAd
    sAd_25 <- FS[[par_name]][,'rAd_25']/xAd
    sAd_50 <- FS[[par_name]][,'rAd_50']/xAd
    sAd_75 <- FS[[par_name]][,'rAd_75']/xAd
    sAd_975 <- FS[[par_name]][,'rAd_975']/xAd
    
    # bad values and smoothing
    sAd_025[is.na(sAd_025)] <- 0.
    sAd_025[sAd_025<1.e-6] <- 0.
    smoothing <- loess(sAd_025 ~ xAd)
    sAd_025 <- smoothing$fitted
    sAd_25[is.na(sAd_25)] <- 0.
    sAd_25[sAd_25<1.e-6] <- 0.
    smoothing <- loess(sAd_25 ~ xAd)
    sAd_25 <- smoothing$fitted
    sAd_50[is.na(sAd_50)] <- 0.
    sAd_50[sAd_50<1.e-6] <- 0.
    smoothing <- loess(sAd_50 ~ xAd)
    sAd_50 <- smoothing$fitted
    sAd_75[is.na(sAd_75)] <- 0.
    sAd_75[sAd_75<1.e-6] <- 0.
    smoothing <- loess(sAd_75 ~ xAd)
    sAd_75 <- smoothing$fitted
    sAd_975[is.na(sAd_975)] <- 0.
    sAd_975[sAd_975<1.e-6] <- 0.
    smoothing <- loess(sAd_975 ~ xAd)
    sAd_975 <- smoothing$fitted
    
    
    dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975,
                        x1, s1_025,s1_25,s1_50,s1_75,s1_975,
                        xAd, sAd_025,sAd_25,sAd_50,sAd_75,sAd_975)
    return(dataF)
}
