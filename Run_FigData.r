### MAJ le 21/06/2016
rm(list = ls())		# Remise à zéro mémoire de la session de R

# REPERTOIRE DE TRAVAIL : A METTRE A JOUR POUR UTILISATION
#setwd("/home/I21149/Modeles/Victor/Archive/")

setwd("/home/I21149/Bureau/Maquette2_0.5/")	


### Parametres
disc<-10 	# Pas de discrétisation
X0x<-110	# Borne max des axes de densités (Utilisation des percentiles (0.999))
X1x<-60
XAdx<-35

# Definition des échelles de visualisation
X0 <- c(c(0.001),sapply(seq(from=(X0x/(disc)),to=X0x, length.out=(disc-1)), round))
X1 <- c(c(0.001),sapply(seq(from=(X1x/(disc)),to=X1x, length.out=(disc-1)), round))
XAd <- c(c(0.001),sapply(seq(from=(XAdx/(disc)),to=XAdx, length.out=(disc-1)), round))


# Gammes des valeurs de paramètres au format c(<min>, <max>, <pas de mesure>)
#T10rg<-c(9.3, 16.6, 1)
#T90rg<-c(1.6, 7.6, 1)
#Crg<-c(0.2, 6.5, 1)
T10rg<-c(9., 17., 0.5)
T90rg<-c(1., 8., 0.5)
Crg<-c(0., 7., 0.5)


Nm<-12	# Nombre de mois entre échantillonnages

generation_data <- function(disc, X0x, X1x, XAdx, T10rg, T90rg, Crg, Nm){
	### INIT
	
		# Chargement des sorties du modèle
	# source(paste("7 R/scripts/Models/MHB",modn,"/Model_",model.type,".txt",sep=""))
	#load(file=paste('data/',model.Name,'_Monit1_summary.par.RData',sep=''))
	load(file=paste('data/MHB1_chains_dataframe.RData',sep=''))
	#M<-read.table(file=paste('data/',model.Name,'_Monit1_parsummary.csv',sep=''), sep=";",dec=",", header=TRUE)
	
	# Creation des repertoirs pour enregistrer les sorties (figures et tables)
	dir.create(path=paste('Outputs/plot/',sep=''), showWarnings = FALSE)
	dir.create(path=paste('Outputs/OutputsCSV/',sep=''), showWarnings = FALSE)
	
	
	# Definition des échelles de visualisation
	#X0<-seq(from=0.001,to=X0x, length.out=disc)
	#X1<-seq(from=0.001,to=X1x, length.out=disc)
	#XAd<-seq(from=0.001,to=XAdx, length.out=disc)

	dmonthUS=3	# Nombre moyen de mois entre pêche et ponte
	dmonthAS=9	# Nombre moyen de mois entre ponte et pêche
	dmonthUE=6	# Nombre moyen de mois entre ponte et emergence
	dmonthAE=6	# Nombre moyen de mois entre emergence et pêche

	samples<-length(as.matrix(df.mcmc[,1]))	# Nombre d'itérations du modèle
	dEch<-rep(0,samples*3); gEch<-rep(0,samples*3); dAdch<-rep(0,samples*3); gAdch<-rep(0,samples*3); 
	d0ch<-rep(0,samples*3); g0ch<-rep(0,samples*3); 
	d1ch<-rep(0,samples*3); g1ch<-rep(0,samples*3); dAdch<-rep(0,samples*3); gAdch<-rep(0,samples*3)
	
	# Chargement des paramètres du modèle (sorties)
	dEgg<- as.matrix(df.mcmc[,'dEgg'])
	psy<- as.matrix(df.mcmc[,'psy'])
	phi<- as.matrix(df.mcmc[,'phi'])
	d1<- as.matrix(df.mcmc[,'d1'])
	dE<- as.matrix(df.mcmc[,'dE'])
	gE<- as.matrix(df.mcmc[,'gE'])
	d0<- as.matrix(df.mcmc[,'d0'])
	g0<- as.matrix(df.mcmc[,'g0'])
	Std0<- as.matrix(df.mcmc[,'Std0'])
	Std1<- as.matrix(df.mcmc[,'Std1'])
	StdAd<- as.matrix(df.mcmc[,'StdAd'])

		# Paramètres hiérarchiques
		## Ordre des paramètres : 'hBarr','Caches','VHAad','L50','T10','T90'
	alpha_g1<- as.matrix(df.mcmc[,'alpha_g1'])
	beta_g1C<- as.matrix(df.mcmc[,'beta_g1[6]'])
	beta_g1T<- as.matrix(df.mcmc[,'beta_g1[2]']) # T90

	alpha_gAd<- as.matrix(df.mcmc[,'alpha_gAd'])
	beta_gAdC<- as.matrix(df.mcmc[,'beta_gAd[2]'])
	beta_gAdT<- as.matrix(df.mcmc[,'beta_gAd[5]']) # T10

	alpha_dAd<- as.matrix(df.mcmc[,'alpha_dAd'])

	sdg1<- as.matrix(df.mcmc[,'sdg1'])	
	sdgAd<- as.matrix(df.mcmc[,'sdgAd'])	
	sddAd<- as.matrix(df.mcmc[,'sddAd'])	


	# pred delta Ad
	lEdAd<-alpha_dAd
	DAd<-exp(rnorm(samples,mean=lEdAd,sd=sddAd))


	################
	## PARAMETRES 
	################
	FS=NULL
	FS[["X0"]]<-X0
	FS[["X1"]]<-X1
	FS[["XAd"]]<-XAd

	FD=NULL

	# Initialisation barre de progression
	Nx=length(seq(from=T10rg[1],to=T10rg[2], by=T10rg[3]))*
		length(seq(from=T90rg[1],to=T90rg[2], by=T90rg[3]))*
		length(seq(from=Crg[1],to=Crg[2], by=Crg[3]))
		#length(X1)*length(XAd)
	
	

	
	pb <- txtProgressBar(min=0, max=Nx, initial=0, title='Generation des données', label='Generation data', style=3)
	
	nrun=0
	
	for (T10 in seq(from=T10rg[1],to=T10rg[2], by=T10rg[3])){
		for (T90 in seq(from=T90rg[1],to=T90rg[2], by=T90rg[3])){
			for (C in seq(from=Crg[1],to=Crg[2], by=Crg[3])){
				# Codification de la combinaison de parametres
				par_name=paste('T10',T10,'T90',T90,'C',C, sep="_")
				T10c<-scale(T10, center=12.99096, scale=1.6245)
				T90c<-scale(T90, center=3.944858, scale=1.256369)
				Cc<-scale(C, center=2.212839, scale=1.445699)

				# pred G1
				lEG1<-alpha_g1 + rep(Cc,samples)*beta_g1C + rep(T90c,samples)*beta_g1T
				G1<-exp(rnorm(samples,mean=lEG1,sd=sdg1))
							
				# pred GAd
				lEGAd<-alpha_gAd + rep(Cc,samples)*beta_gAdC + rep(T10c,samples)*beta_gAdT
				##	lEGAd<-alpha_gAd + rep(Cc,samples)*beta_g1C + rep(T10c,samples)*beta_gAdT
				GAd<-exp(rnorm(samples,mean=lEGAd,sd=sdgAd))
						
						
				#########################################
				### FIGURE 1 : TAUX DE SURVIE
				#########################################

				FS[[par_name]]<-as.data.frame(matrix(NA,ncol=15,nrow=length(X0)))
				colnames(FS[[par_name]])<-c('r1_025','r1_25','r1_50','r1_75','r1_975',
										'r2_025','r2_25','r2_50','r2_75','r2_975',
										'rAd_025','rAd_25','rAd_50','rAd_75','rAd_975'
										)
						
						
				# Initialisation des variables de prédictions et des domaines de proba
				for(xi in 1:disc){
					Dp1_trans01 <-exp(-(Nm-dmonthAE)*d0)*X0[xi]/(1+(g0/d0*(1-exp(-(Nm-dmonthAE)*d0))*X0[xi]))
					Dp1<-Dp1_trans01*exp(-dmonthAE*d1)/(1+(G1/d1*(1-exp(-dmonthAE*d1))*Dp1_trans01))

					Dpe1<-Dp1*exp(rnorm(n=samples, mean=0,sd=Std1))
					FS[[par_name]][xi,'r1_025'] <- quantile(Dpe1, 0.025)
					FS[[par_name]][xi,'r1_25'] <- quantile(Dpe1, 0.25)
					FS[[par_name]][xi,'r1_50'] <- quantile(Dpe1, 0.5)
					FS[[par_name]][xi,'r1_75'] <- quantile(Dpe1, 0.75)
					FS[[par_name]][xi,'r1_975'] <- quantile(Dpe1, 0.975)
									
					Dp2_trans12 <-exp(-(Nm-dmonthAE)*d1)*X1[xi]/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*X1[xi]))
					Dp2<-Dp2_trans12*exp(-dmonthAE*DAd)/(1+(GAd/DAd*(1-exp(-dmonthAE*d1))*Dp2_trans12))

					Dpe2<-Dp2*exp(rnorm(n=samples, mean=0,sd=StdAd))	# StdAd estimé pour l'erreur sur la somme des 2+ et >2+ --> Enveloppe d'incertitude surestimée sur cette visualisation
					FS[[par_name]][xi,'r2_025'] <- quantile(Dpe2, 0.025)
					FS[[par_name]][xi,'r2_25'] <- quantile(Dpe2, 0.25)
					FS[[par_name]][xi,'r2_50'] <- quantile(Dpe2, 0.5)
					FS[[par_name]][xi,'r2_75'] <- quantile(Dpe2, 0.75)
					FS[[par_name]][xi,'r2_975'] <- quantile(Dpe2, 0.975)

					DpAd<-XAd[xi]*exp(-Nm*DAd)/(1+(GAd/DAd*(1-exp(-Nm*DAd))*XAd[xi]))
					DpeAd<-DpAd*exp(rnorm(n=samples, mean=0,sd=StdAd))	# StdAd estimé pour l'erreur sur la somme des 2+ et >2+ --> Enveloppe d'incertitude surestimée sur cette visualisation
					FS[[par_name]][xi,'rAd_025'] <- quantile(DpeAd, 0.025)
					FS[[par_name]][xi,'rAd_25'] <- quantile(DpeAd, 0.25)
					FS[[par_name]][xi,'rAd_50'] <- quantile(DpeAd, 0.5)
					FS[[par_name]][xi,'rAd_75'] <- quantile(DpeAd, 0.75)
					FS[[par_name]][xi,'rAd_975'] <- quantile(DpeAd, 0.975)
					}
							
						
				## + Moyenne mobile pour lisser simu?	
								
				#########################################
				### FIGURE 3 : Niveau de Population
				#########################################
				# Dependant d'une valeur de X1m et XAdm (densité d'1+ ou d'Ad fixé pour l'année y-1)
				for (X1m in  X1){
					for (XAdm in  XAd){
						par_name_full=paste('T10',T10,'T90',T90,'C',C,'X1m',X1m,'XAdm',XAdm, sep="_")
						
						FS[[par_name_full]]<-as.data.frame(matrix(NA,ncol=10,nrow=length(X0)))
						colnames(FS[[par_name_full]])<-c('r2_025_Adm','r2_25_Adm','r2_50_Adm','r2_75_Adm','r2_975_Adm',
												'rAd_025_1m','rAd_25_1m','rAd_50_1m','rAd_75_1m','rAd_975_1m'
												)
						
						Dp2_trans1m2m <-exp(-(Nm-dmonthAE)*d1)*X1m/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*X1m))
						Dp2m<-Dp2_trans1m2m*exp(-dmonthAE*DAd)/(1+(GAd/DAd*(1-exp(-dmonthAE*d1))*Dp2_trans12))
						
						DpAdm<-XAdm*exp(-Nm*DAd)/(1+(GAd/DAd*(1-exp(-Nm*DAd))*XAdm))
						
						for(xi in 1:disc){
							Dp2_trans12 <-exp(-(Nm-dmonthAE)*d1)*X1[xi]/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*X1[xi]))
							Dp2<-Dp2_trans12*exp(-dmonthAE*DAd)/(1+(GAd/DAd*(1-exp(-dmonthAE*d1))*Dp2_trans12))
							DpAd<-XAd[xi]*exp(-Nm*DAd)/(1+(GAd/DAd*(1-exp(-Nm*DAd))*XAd[xi]))
							
							DpeAd_Adm<-(DpAdm+Dp2)*exp(rnorm(n=samples, mean=0,sd=StdAd))	# StdAd estimé pour l'erreur sur la somme des 2+ et >2+ --> Enveloppe d'incertitude surestimée sur cette visualisation
							FS[[par_name_full]][xi,'r2_025_Adm'] <- quantile(DpeAd_Adm, 0.025)
							FS[[par_name_full]][xi,'r2_25_Adm'] <- quantile(DpeAd_Adm, 0.25)
							FS[[par_name_full]][xi,'r2_50_Adm'] <- quantile(DpeAd_Adm, 0.50)
							FS[[par_name_full]][xi,'r2_75_Adm'] <- quantile(DpeAd_Adm, 0.75)
							FS[[par_name_full]][xi,'r2_975_Adm'] <- quantile(DpeAd_Adm, 0.975)
							
							DpeAd_1m<-(DpAd+Dp2m)*exp(rnorm(n=samples, mean=0,sd=StdAd))	# StdAd estimé pour l'erreur sur la somme des 2+ et >2+ --> Enveloppe d'incertitude surestimée sur cette visualisation
							FS[[par_name_full]][xi,'rAd_025_1m'] <- quantile(DpeAd_1m, 0.025)
							FS[[par_name_full]][xi,'rAd_25_1m'] <- quantile(DpeAd_1m, 0.25)
							FS[[par_name_full]][xi,'rAd_50_1m'] <- quantile(DpeAd_1m, 0.50)
							FS[[par_name_full]][xi,'rAd_75_1m'] <- quantile(DpeAd_1m, 0.75)
							FS[[par_name_full]][xi,'rAd_975_1m'] <- quantile(DpeAd_1m, 0.975)
							}

						}
					}
				
				#########################################
				### FIGURE 2 : Niveau de Population
				#########################################
						
				## Distribution des densités de 0+ en entrée pour la figure densités
				X0pred <-rlnorm(samples,meanlog=2.2, sdlog=0.9)	# Distribution des niveaux de recrutements globalement observés sur les 40 stations du modèle
				X0predl<-rnorm(samples,mean=3, sd=0.2)	# Distribution de recrutements faibles (E = 3.7 ind/100m-2)
				X0predm<-rnorm(samples,mean=9, sd=0.2)	# Distribution de recrutements faibles à moyens (E = 10.3 ind/100m-2)
				X0predh<-rnorm(samples,mean=15, sd=0.2)	# Distribution de recrutements moyens à forts (E = 27.7 ind/100m-2)
						
						
				SEQ<-seq(from=0,to=1000,length.out=1000)
				FD[[par_name]]<-as.data.frame(matrix(NA,ncol=6,nrow=length(SEQ)-1))
				colnames(FD[[par_name]])<-c('D1l','D1m','D1h','DAdl','DAdm','DAdh')
				FD[['xinf']]<-SEQ[-length(SEQ)]
				FD[['X0l']]<-hist(X0predl,breaks=SEQ,plot=FALSE)$density
				FD[['X0m']]<-hist(X0predm,breaks=SEQ,plot=FALSE)$density
				FD[['X0h']]<-hist(X0predh,breaks=SEQ,plot=FALSE)$density

				for(k in 1:4){
					Dpred<-as.data.frame(matrix(0,ncol=9,nrow=samples))
					colnames(Dpred)<-c('X0','Dp1tr','Dp1','Dpe1','Dp2tr','DpSup2tr','DpAdtr','DpAd','DpeAd')

					if(k==1){Dpred$X0<-X0pred}
					if(k==2){Dpred$X0<-X0predl}
					if(k==3){Dpred$X0<-X0predm}
					if(k==4){Dpred$X0<-X0predh}

					Dpred[,'Dp1tr']<-exp(-(Nm-dmonthAE)*d0)*Dpred[,'X0']/(1+(g0/d0*(1-exp(-(Nm-dmonthAE)*d0))*Dpred[,'X0']))
					Dpred[,'Dp1']<-Dpred[,'Dp1tr']*exp(-dmonthAE*d1)/(1+(G1/d1*(1-exp(-dmonthAE*d1))*Dpred[,'Dp1tr']))
					Dpred[,'Dpe1']<-Dpred[,'Dp1']*exp(rnorm(n=1, mean=0,sd=Std1))
					Dpred[,'Dp2tr'] <-exp(-(Nm-dmonthAE)*d1)*Dpred[,'Dp1']/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*Dpred[,'Dp1']))


					# On initialise les estimations des adultes (arrivés initiales de 2+ uniquement, puis utilisation des cohortes successives (utilisation des 20 premières lignes pour s'approcher d'un DAd à l'équilibre)
					Dpred[1,'DpAdtr'] <-Dpred[1,'Dp2tr']
					Dpred[1,'DpAd'] <-Dpred[1,'DpAdtr']*exp(-dmonthAE*DAd[1])/(1+(GAd[1]/DAd[1]*(1-exp(-dmonthAE*DAd[1]))*Dpred[1,'DpAdtr']))

					#### Vectorisation parait impossible pour cette partie :
						# On utilise les densités d'adultes de la simulation précédente (approximée comme une 'année précédente') pour arriver après quelques simulation a un niveau d'une population à l'équilibre.
						# Travailler en vectorisant ne fait pas intervenir la DDep entre adultes de manière 
					for (ii in 2:samples){
						Dpred[ii,'DpSup2tr'] <- exp(-(Nm-dmonthAE)*DAd[ii])*Dpred[ii-1,'DpAd']/(1+(GAd[ii]/DAd[ii]*(1-exp(-(Nm-dmonthAE)*DAd[ii]))*Dpred[ii-1,'DpAd']))
						Dpred[ii,'DpAdtr'] <-Dpred[ii,'Dp2tr'] + Dpred[ii,'DpSup2tr']
						Dpred[ii,'DpAd'] <-Dpred[ii,'DpAdtr']*exp(-dmonthAE*DAd[ii])/(1+(GAd[ii]/DAd[ii]*(1-exp(-dmonthAE*DAd[ii]))*Dpred[ii,'DpAdtr']))

						Dpred[ii,'DpeAd']<-Dpred[ii,'DpAd']*exp(rnorm(n=1, mean=0,sd=StdAd[ii]))	# StdAd estimé pour l'erreur sur la somme des 2+ et >2+ --> Enveloppe d'incertitude surestimée sur cette visualisation

						}

					# Sauvegarde des distributions
					if(k==1){Dpredall<-Dpred}
					if(k==2){FD[[par_name]]$D1l<-hist(Dpred$Dpe1,breaks=SEQ,plot=FALSE)$density; FD[[par_name]]$DAdl<-hist(Dpred[-c(1:10),'DpeAd'],breaks=SEQ,plot=FALSE)$density}
					if(k==3){FD[[par_name]]$D1m<-hist(Dpred$Dpe1,breaks=SEQ,plot=FALSE)$density; FD[[par_name]]$DAdm<-hist(Dpred[-c(1:10),'DpeAd'],breaks=SEQ,plot=FALSE)$density}
					if(k==4){FD[[par_name]]$D1h<-hist(Dpred$Dpe1,breaks=SEQ,plot=FALSE)$density; FD[[par_name]]$DAdh<-hist(Dpred[-c(1:10),'DpeAd'],breaks=SEQ,plot=FALSE)$density}
					}




							
							
				#save(FS, file='data/FS.RData')
				#save(FD, file='data/FD.RData')

				nrun=nrun+1
				setTxtProgressBar(pb, nrun)

				}
			}
		}
	save(FS, file='data/FS.RData')
	save(FD, file='data/FD.RData')
	close(pb)

	}


generation_data(disc, X0x, X1x, XAdx, T10rg, T90rg, Crg, Nm)

