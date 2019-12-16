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
### Fonction de creation de la FIGURE 2 : RELATION type 'SR' : densité d'une classe d'âge l'année y en fonction des densités de la classe d'âge précédente l'année y-1
#### Integration d'un niveau de densité 1+ ou Ad influençant la survie
#########################################
	# ids : nom de la station (utilisé pour le titre)
	# FS : Données chargée (correspondant au set de paramètres en entrée)
	# T10, T90, C : Valeur des paramètres d'entrées du modèle (caractéristiques de la station T10 annuelle, T90 annuelle et % de S de caches)
	# X1m, XAdm : Valeur de 
	# [temp] OS : type d'OS (adapte les commandes d'ouverture de fenetres graphique
	
fig2_p0_export <- function(ids, FS, t_10, t_90, cache)
{
	par_name=paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
	#par_name_full=paste('T10',t_10,'T90',t_90,'C',cache,'X1m',X1m,'XAdm',XAdm, sep="_")

	# Mise en forme donnees
	x0 <- FS[["X0"]]
	s0_025 <- FS[[par_name]][,'r1_025']
	s0_25 <- FS[[par_name]][,'r1_25']
	s0_50 <- FS[[par_name]][,'r1_50']
	s0_75 <- FS[[par_name]][,'r1_75']
	s0_975 <- FS[[par_name]][,'r1_975']
	
	# bad values and smoothing
	threshold <- 1.e-8
	s0_025[is.na(s0_025)] <- 0.
	s0_025[s0_025<threshold] <- 0.
	smoothing <- loess(s0_025 ~ x0)
	s0_025 <- smoothing$fitted
	s0_25[is.na(s0_25)] <- 0.
	s0_25[s0_25<threshold] <- 0.
	smoothing <- loess(s0_25 ~ x0)
	s0_25 <- smoothing$fitted
	s0_50[is.na(s0_50)] <- 0.
	s0_50[s0_50<threshold] <- 0.
	smoothing <- loess(s0_50 ~ x0)
	s0_50 <- smoothing$fitted
	s0_75[is.na(s0_75)] <- 0.
	s0_75[s0_75<threshold] <- 0.
	smoothing <- loess(s0_75 ~ x0)
	s0_75 <- smoothing$fitted
	s0_975[is.na(s0_975)] <- 0.
	s0_975[s0_975<threshold] <- 0.
	smoothing <- loess(s0_975 ~ x0)
	s0_975 <- smoothing$fitted
	
	dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975)
		
	return(dataF)
}


# type = "heatmap" or "surface"

fig2_hm_export <- function(ids, FS, t_10, t_90, cache, X1m, XAdm, type3D=FALSE)
{
	par_name=paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
	#par_name_full=paste('T10',t_10,'T90',t_90,'C',cache,'X1m',X1m,'XAdm',XAdm, sep="_")
	disc=length(FS[["X0"]])
	
	# Mise en forme données
	x0 <- FS[["X0"]]
	s0_025 <- FS[[par_name]][,'r1_025']
	s0_25 <- FS[[par_name]][,'r1_25']
	s0_50 <- FS[[par_name]][,'r1_50']
	s0_75 <- FS[[par_name]][,'r1_75']
	s0_975 <- FS[[par_name]][,'r1_975']
	
	# bad values and smoothing
	threshold <- 1.e-8
	s0_025[is.na(s0_025)] <- 0.
	s0_025[s0_025<threshold] <- 0.
	smoothing <- loess(s0_025 ~ x0)
	s0_025 <- smoothing$fitted
	s0_25[is.na(s0_25)] <- 0.
	s0_25[s0_25<threshold] <- 0.
	smoothing <- loess(s0_25 ~ x0)
	s0_25 <- smoothing$fitted
	s0_50[is.na(s0_50)] <- 0.
	s0_50[s0_50<threshold] <- 0.
	smoothing <- loess(s0_50 ~ x0)
	s0_50 <- smoothing$fitted
	s0_75[is.na(s0_75)] <- 0.
	s0_75[s0_75<threshold] <- 0.
	smoothing <- loess(s0_75 ~ x0)
	s0_75 <- smoothing$fitted
	s0_975[is.na(s0_975)] <- 0.
	s0_975[s0_975<threshold] <- 0.
	smoothing <- loess(s0_975 ~ x0)
	s0_975 <- smoothing$fitted
	
	# loading FS_PopLvl
	if(Sys.info()["sysname"] == "Darwin"){
	  load(file=paste('../data/FS_PopLvl/FS_PopLvl_',par_name,'.RData',sep=''))
	}else{
	  load(file=paste('/home/dypop/data/FS_PopLvl/FS_PopLvl_',par_name,'.RData',sep=''))
	}
	
	x1 <- FS[["X1"]]
	s1_025 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_025']
	s1_25 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_25']
	s1_50 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_50']
	s1_75 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_75']
	s1_975 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_975']

	# bad values and smoothing
	s1_025[is.na(s1_025)] <- 0.
	s1_025[s1_025<threshold] <- 0.
	smoothing <- loess(s1_025 ~ x1)
	s1_025 <- smoothing$fitted
	s1_25[is.na(s1_25)] <- 0.
	s1_25[s1_25<threshold] <- 0.
	smoothing <- loess(s1_25 ~ x1)
	s1_25 <- smoothing$fitted
	s1_50[is.na(s1_50)] <- 0.
	s1_50[s1_50<threshold] <- 0.
	smoothing <- loess(s1_50 ~ x1)
	s1_50 <- smoothing$fitted
	s1_75[is.na(s1_75)] <- 0.
	s1_75[s1_75<threshold] <- 0.
	smoothing <- loess(s1_75 ~ x1)
	s1_75 <- smoothing$fitted
	s1_975[is.na(s1_975)] <- 0.
	s1_975[s1_975<threshold] <- 0.
	smoothing <- loess(s1_975 ~ x1)
	s1_975 <- smoothing$fitted
	
	xAd <- FS[["XAd"]]
	sAd_025 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_025']
	sAd_25 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_25']
	sAd_50 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_50']
	sAd_75 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_75']
	sAd_975 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_975']

	# bad values and smoothing
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
	
	dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975,
		x1, s1_025,s1_25,s1_50,s1_75,s1_975, 
		xAd, sAd_025,sAd_25,sAd_50,sAd_75,sAd_975)
		
	return(dataF)
	}

