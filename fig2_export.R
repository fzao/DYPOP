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
	
	
	# loading FS_PopLvl
	load(file=paste('data/FS_PopLvl/FS_PopLvl_',par_name,'.RData',sep=''))
	
	
	x1 <- FS[["X1"]]
	s1_025 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_025']
	s1_25 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_25']
	s1_50 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_50']
	s1_75 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_75']
	s1_975 <- FS_PopLvl[[paste("XAdm_",XAdm,sep='')]][,'rAd_975']

	xAd <- FS[["XAd"]]
	sAd_025 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_025']
	sAd_25 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_25']
	sAd_50 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_50']
	sAd_75 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_75']
	sAd_975 <- FS_PopLvl[[paste("X1m_",X1m,sep='')]][,'rAd_975']

	dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975,
		x1, s1_025,s1_25,s1_50,s1_75,s1_975, 
		xAd, sAd_025,sAd_25,sAd_50,sAd_75,sAd_975)
		
	return(dataF)
	}

