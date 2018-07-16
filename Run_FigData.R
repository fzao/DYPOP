# REPERTOIRE DE TRAVAIL : A METTRE A JOUR POUR UTILISATION
#setwd("/home/I21149/Modeles/Victor/Archive/")
#OSu='linux'

#setwd("C:/Users/Victor/Documents/0 W/Interf_MHB_github/DynFish")	

# Data generation for the figures
rm(list = ls())		# Remise à zéro mémoire de la session de R
# Parameters
disc <- 30 	# number of even-spaced x-axis ticks
X0x  <- 110	# Bounds for density axes (use of percentiles (0.999))
X1x  <- 60
XAdx <- 35
# Scales for the visualization
X0 <- c(c(0.1), sapply(seq(from=(X0x/(disc)),to=X0x, length.out=(disc-1)), round))
X1 <- c(c(0.1), sapply(seq(from=(X1x/(disc)),to=X1x, length.out=(disc-1)), round))
XAd <- c(c(0.1), sapply(seq(from=(XAdx/(disc)),to=XAdx, length.out=(disc-1)), round))
# Desired values c(<min>, <max>, <step>)
stepsize <- 1.
T10rg <- c(9., 17., stepsize)
T90rg <- c(1., 8., stepsize)
Crg <- c(0., 7., stepsize)
# Number of months for the sampling
Nm <- 12


# Get moving average of a vector
# x: the vector
# n: the number of samples
# centered: if FALSE, then average current sample and previous (n-1) samples
#           if TRUE, then average symmetrically in past and future. (If n is even, use one more sample from future.)
movingAverage <- function(x, n=1, centered=TRUE) {
    
    if (centered) {
        before <- floor  ((n-1)/2)
        after  <- ceiling((n-1)/2)
    } else {
        before <- n-1
        after  <- 0
    }

    # Track the sum and count of number of non-NA items
    s     <- rep(0, length(x))
    count <- rep(0, length(x))
    
    # Add the centered data 
    new <- x
    # Add to count list wherever there isn't a 
    count <- count + !is.na(new)
    # Now replace NA_s with 0_s and add to total
    new[is.na(new)] <- 0
    s <- s + new
    
    # Add the data from before
    i <- 1
    while (i <= before) {
        # This is the vector with offset values to add
        new   <- c(rep(NA, i), x[1:(length(x)-i)])

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        
        i <- i+1
    }

    # Add the data from after
    i <- 1
    while (i <= after) {
        # This is the vector with offset values to add
        new   <- c(x[(i+1):length(x)], rep(NA, i))
       
        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        
        i <- i+1
    }
    
    # return sum divided by count
    s/count
}

# Creating data
	# disc : number of even-spaced x-axis ticks
	# X0, X1, XAd : vectors of x-axis ticks for 0+, 1+ and Ad
	# t_10_L, t_90_L, cache_L : vectors of input parameters to use for prediction
	# Nm : Number of months for the sampling
	# existing_FS_data_path : path to an (optional) FS data to use at initialization. Running time will be reduced if some inputs data were already used to create this FD
		# FD data will also be loaded (path inferred from existing_FS_data_path)
		# x-axis data (X0, X1; XAd) have to be equals. An Error will be returned otherwise.
	# name_suff : string to paste at the end of 'FS' and 'FD' when saving the RData (output will be saved as 'FS<name_suff>.RData')
generation_data <- function(disc, X0, X1, XAd, t_10_L, t_90_L, cache_L, Nm=12, existing_FS_data_path=NULL, name_suff=''){
	# Loading model results
	load(file=paste('data/MHB1_chains_dataframe.RData',sep=''))
	# Output directories for the results
	dir.create(path=paste('Outputs/plot/',sep=''), showWarnings = FALSE)
	#dir.create(path=paste('Outputs/OutputsCSV/',sep=''), showWarnings = FALSE)
	dir.create(path=paste('data/FS_PopLvl/',sep=''), showWarnings = FALSE)
    # Periods
	dmonthUS=3	# Mean number of months between fishing and spawning
	dmonthAS=9	# Mean number of months between spawning and fishing
	dmonthUE=6	# Mean number of months between fishing and emergence
	dmonthAE=6	# Mean number of months between emergence and fishing

	samples <- length(as.matrix(df.mcmc[,1]))  # Number of iterations for the model
	dEch <- rep(0,samples*3); gEch<-rep(0,samples*3)
    dAdch <- rep(0,samples*3); gAdch<-rep(0,samples*3)
	d0ch <- rep(0,samples*3); g0ch<-rep(0,samples*3)
	d1ch <- rep(0,samples*3); g1ch<-rep(0,samples*3)
    dAdch <- rep(0,samples*3); gAdch<-rep(0,samples*3)

	# Loading model parameters (outputs)
	dEgg <- as.matrix(df.mcmc[,'dEgg'])
	psy <- as.matrix(df.mcmc[,'psy'])
	phi <- as.matrix(df.mcmc[,'phi'])
	d1 <- as.matrix(df.mcmc[,'d1'])
	dE <- as.matrix(df.mcmc[,'dE'])
	gE <- as.matrix(df.mcmc[,'gE'])
	d0 <- as.matrix(df.mcmc[,'d0'])
	g0 <- as.matrix(df.mcmc[,'g0'])
	Std0 <- as.matrix(df.mcmc[,'Std0'])
	Std1 <- as.matrix(df.mcmc[,'Std1'])
	StdAd <- as.matrix(df.mcmc[,'StdAd'])

    # Hierarchical parameters
    ## Orders of parameters: 'hBarr','Caches','VHAad','L50','T10','T90'
	alpha_g1 <- as.matrix(df.mcmc[,'alpha_g1'])
	beta_g1C <- as.matrix(df.mcmc[,'beta_g1[6]'])
	beta_g1T <- as.matrix(df.mcmc[,'beta_g1[2]']) # T90

	alpha_gAd <- as.matrix(df.mcmc[,'alpha_gAd'])
	beta_gAdC <- as.matrix(df.mcmc[,'beta_gAd[2]'])
	beta_gAdT <- as.matrix(df.mcmc[,'beta_gAd[5]']) # T10

	alpha_dAd <- as.matrix(df.mcmc[,'alpha_dAd'])

	sdg1 <- as.matrix(df.mcmc[,'sdg1'])
	sdgAd <- as.matrix(df.mcmc[,'sdgAd'])
	sddAd <- as.matrix(df.mcmc[,'sddAd'])

    # pred delta Ad
	lEdAd <- alpha_dAd
	DAd <- exp(rnorm(samples, mean=lEdAd, sd=sddAd))

	#############
	## PARAMETERS
	#############
	if (is.null(existing_FS_data_path)){
		FS = NULL
		FS[["X0"]] <- X0
		FS[["X1"]] <- X1
		FS[["XAd"]] <- XAd
		
		FD = NULL
		}
	# If existing data is used, we check that x-axis are the same
	if (!is.null(existing_FS_data_path)){
		# Loading pre-existing data
		load(file=existing_FS_data_path)
		load(file=gsub("FS", "FD", existing_FS_data_path)) # Path to FD is inferred from FS path.
		
		if (sum(FS[["X0"]]==X0)!=length(X0)){
			stop('Loaded data (',existing_FS_data_path,') does not have the same x-axis than X0 !')
			}
		}
	

	# Progressbar initialization
	Nx=length(t_10_L)*
		length(t_90_L)*
		length(cache_L)

	pb <- txtProgressBar(min=0, max=Nx, initial=0, title='Generation des données', label='Generation data', style=3)
	nrun=0
	
	

	for (t_10 in t_10_L){
		for (t_90 in t_90_L){
			for (cache in cache_L){
				# Codification de la combinaison de parametres
				par_name=paste('T10', t_10,'T90', t_90,'C', cache, sep="_")
				
				# If those element are already in the loaded FS, they are not recreated
				if (! par_name %in% names(FS)){
					T10c<-scale(t_10, center=12.99096, scale=1.6245)
					T90c<-scale(t_90, center=3.944858, scale=1.256369)
					Cc<-scale(cache, center=2.212839, scale=1.445699)

					# pred G1
					lEG1<-alpha_g1 + rep(Cc,samples)*beta_g1C + rep(T90c,samples)*beta_g1T
					G1<-exp(rnorm(samples,mean=lEG1,sd=sdg1))

					# pred GAd
					lEGAd<-alpha_gAd + rep(Cc,samples)*beta_gAdC + rep(T10c,samples)*beta_gAdT
					##	lEGAd<-alpha_gAd + rep(Cc,samples)*beta_g1C + rep(T10c,samples)*beta_gAdT
					GAd<-exp(rnorm(samples,mean=lEGAd,sd=sdgAd))

					#########################################
					### FIGURE 1 : SURVIVAL RATE
					#########################################
					FS[[par_name]] <- as.data.frame(matrix(NA,ncol=15,nrow=length(X0)))
					colnames(FS[[par_name]]) <- c('r1_025','r1_25','r1_50','r1_75','r1_975',
											'r2_025','r2_25','r2_50','r2_75','r2_975',
											'rAd_025','rAd_25','rAd_50','rAd_75','rAd_975'
											)

					# Initialization of prediction variables and probability domains
					for(xi in 1:disc){
						Dp1_trans01 <- exp(-(Nm-dmonthAE)*d0)*X0[xi]/(1+(g0/d0*(1-exp(-(Nm-dmonthAE)*d0))*X0[xi]))
						Dp1 <- Dp1_trans01*exp(-dmonthAE*d1)/(1+(G1/d1*(1-exp(-dmonthAE*d1))*Dp1_trans01))

						Dpe1 <- Dp1*exp(rnorm(n=samples, mean=0,sd=Std1))
						FS[[par_name]][xi,'r1_025'] <- quantile(Dpe1, 0.025)
						FS[[par_name]][xi,'r1_25'] <- quantile(Dpe1, 0.25)
						FS[[par_name]][xi,'r1_50'] <- quantile(Dpe1, 0.5)
						FS[[par_name]][xi,'r1_75'] <- quantile(Dpe1, 0.75)
						FS[[par_name]][xi,'r1_975'] <- quantile(Dpe1, 0.975)

						Dp2_trans12 <- exp(-(Nm-dmonthAE)*d1)*X1[xi]/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*X1[xi]))
						Dp2 <- Dp2_trans12*exp(-dmonthAE*DAd)/(1+(GAd/DAd*(1-exp(-dmonthAE*d1))*Dp2_trans12))

						Dpe2 <- Dp2*exp(rnorm(n=samples, mean=0,sd=StdAd))	# Estimated StdAd for error on sum of 2+ and> 2+ -> Overestimated uncertainty envelope on this visualization
						FS[[par_name]][xi,'r2_025'] <- quantile(Dpe2, 0.025)
						FS[[par_name]][xi,'r2_25'] <- quantile(Dpe2, 0.25)
						FS[[par_name]][xi,'r2_50'] <- quantile(Dpe2, 0.5)
						FS[[par_name]][xi,'r2_75'] <- quantile(Dpe2, 0.75)
						FS[[par_name]][xi,'r2_975'] <- quantile(Dpe2, 0.975)

						DpAd<-XAd[xi]*exp(-Nm*DAd)/(1+(GAd/DAd*(1-exp(-Nm*DAd))*XAd[xi]))
						DpeAd<-DpAd*exp(rnorm(n=samples, mean=0,sd=StdAd))	# Estimated StdAd for error on sum of 2+ and> 2+ -> Overestimated uncertainty envelope on this visualization
						FS[[par_name]][xi,'rAd_025'] <- quantile(DpeAd, 0.025)
						FS[[par_name]][xi,'rAd_25'] <- quantile(DpeAd, 0.25)
						FS[[par_name]][xi,'rAd_50'] <- quantile(DpeAd, 0.5)
						FS[[par_name]][xi,'rAd_75'] <- quantile(DpeAd, 0.75)
						FS[[par_name]][xi,'rAd_975'] <- quantile(DpeAd, 0.975)
						}

					
					## Moving average to smooth the curve
					if (disc>20){
						FS[[par_name]]=data.frame(apply(FS[[par_name]], 2, movingAverage, n=ceiling(0.05*disc)))
						}
					
					#########################################
					### FIGURE 3 : Population level
					#########################################
					# Dependent on a value of X1m and XAdm (density of 1 + or Ad fixed for year y-1)
					# Saving detailled predictions of survival on FS_PopLvl
					FS_PopLvl = NULL
					for (X1m in  X1){
						par_PopLvl <- paste('X1m',X1m, sep="_")
						
						FS_PopLvl[[par_PopLvl]] <- as.data.frame(matrix(NA,ncol=5,nrow=length(X0)))
						colnames(FS_PopLvl[[par_PopLvl]]) <- c('rAd_025','rAd_25','rAd_50','rAd_75','rAd_975')

						
						Dp2_trans1m2m <- exp(-(Nm-dmonthAE)*d1)*X1m/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*X1m))
						Dp2m <- Dp2_trans1m2m*exp(-dmonthAE*DAd)/(1+(GAd/DAd*(1-exp(-dmonthAE*d1))*Dp2_trans1m2m))
						
						for(xi in 1:disc){
							DpAd <- XAd[xi]*exp(-Nm*DAd)/(1+(GAd/DAd*(1-exp(-Nm*DAd))*XAd[xi]))
							
							DpeAd_1m<-(DpAd+Dp2m)*exp(rnorm(n=samples, mean=0,sd=StdAd))  # Estimated StdAd for error on sum of 2+ and> 2+ -> Overestimated uncertainty envelope on this visualization
							FS_PopLvl[[par_PopLvl]][xi,'rAd_025'] <- quantile(DpeAd_1m, 0.025)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_25'] <- quantile(DpeAd_1m, 0.25)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_50'] <- quantile(DpeAd_1m, 0.50)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_75'] <- quantile(DpeAd_1m, 0.75)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_975'] <- quantile(DpeAd_1m, 0.975)
								
							}
						## Moving average to smooth the curve
						if (disc>20){
							FS_PopLvl[[par_PopLvl]]=data.frame(apply(FS_PopLvl[[par_PopLvl]], 2, movingAverage, n=ceiling(0.05*disc)))
							}
						}
								
					for (XAdm in  XAd){
						par_PopLvl <- paste('XAdm',XAdm, sep="_")
							
						FS_PopLvl[[par_PopLvl]] <- as.data.frame(matrix(NA,ncol=5,nrow=length(X0)))
						colnames(FS_PopLvl[[par_PopLvl]]) <- c('rAd_025','rAd_25','rAd_50','rAd_75','rAd_975')

						DpAdm <- XAdm*exp(-Nm*DAd)/(1+(GAd/DAd*(1-exp(-Nm*DAd))*XAdm))

						for(xi in 1:disc){
							Dp2_trans12 <- exp(-(Nm-dmonthAE)*d1)*X1[xi]/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*X1[xi]))
							Dp2 <- Dp2_trans12*exp(-dmonthAE*DAd)/(1+(GAd/DAd*(1-exp(-dmonthAE*d1))*Dp2_trans12))
							
							DpeAd_Adm <- (DpAdm+Dp2)*exp(rnorm(n=samples, mean=0,sd=StdAd))	# Estimated StdAd for error on sum of 2+ and> 2+ -> Overestimated uncertainty envelope on this visualization
							FS_PopLvl[[par_PopLvl]][xi,'rAd_025'] <- quantile(DpeAd_Adm, 0.025)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_25'] <- quantile(DpeAd_Adm, 0.25)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_50'] <- quantile(DpeAd_Adm, 0.50)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_75'] <- quantile(DpeAd_Adm, 0.75)
							FS_PopLvl[[par_PopLvl]][xi,'rAd_975'] <- quantile(DpeAd_Adm, 0.975)
							
								
							}
								
						## Moving average to smooth the curve
						if (disc>20){
							FS_PopLvl[[par_PopLvl]]=data.frame(apply(FS_PopLvl[[par_PopLvl]], 2, movingAverage, n=ceiling(0.05*disc)))
							}	
						}
						
					# Summary of FS_PopLvl
					sumFSPop=as.data.frame(matrix(0,ncol=length(XAd),nrow=length(X1)))
					colnames(sumFSPop)=XAd
					rownames(sumFSPop)=X1
					
					for(xi in 1:disc){
						x1=X1[xi]
						for(xj in 1:disc){
							xAd=XAd[xj]
							sumFSPop[xi,xj]=FS_PopLvl[[paste('XAdm',XAdm, sep="_")]][xi,'rAd_50']
							}
						}
					
					FS[[paste(par_name,'_2D',sep='')]]=sumFSPop
					
					## Saving detailled results in sub-folder
					save(FS_PopLvl, file=paste('data/FS_PopLvl/FS_PopLvl_',par_name,'.RData', sep=''))
							

					#########################################
					### FIGURE 2 : Population level
					#########################################
					## Distribution of densities of 0+ in input for the figure densities
					X0x=max(X0)
					X0pred <-sapply(rlnorm(samples,meanlog=2.2, sdlog=0.9), function(x){max(min(X0),min(x,X0x))})
					X0predl<-sapply(rnorm(samples,mean=3, sd=1), function(x){max(min(X0),min(x,X0x))})	# Distribution de recrutements faibles (E = 3.7 ind/100m-2)
					X0predm<-sapply(rnorm(samples,mean=15, sd=1), function(x){max(min(X0),min(x,X0x))})	# Distribution de recrutements faibles à moyens (E = 10.3 ind/100m-2)
					X0predh<-sapply(rnorm(samples,mean=60, sd=1), function(x){max(min(X0),min(x,X0x))})	# Distribution de recrutements moyens à forts (E = 27.7 ind/100m-2)
					
					
					SEQ <- X0
					SEQ0 <- c(0,X0[-1])
					FD[[par_name]] <- as.data.frame(matrix(NA, ncol=6, nrow=length(SEQ)-1))
					colnames(FD[[par_name]]) <- c('D1l', 'D1m', 'D1h', 'DAdl', 'DAdm', 'DAdh')
					FD[['xinf']] <- SEQ[-length(SEQ)]
					FD[['X0l']] <- hist(X0predl, breaks=SEQ, plot=FALSE)$density
					FD[['X0m']] <- hist(X0predm, breaks=SEQ, plot=FALSE)$density
					FD[['X0h']] <- hist(X0predh, breaks=SEQ, plot=FALSE)$density

					for(k in 2:4){
						Dpred <- as.data.frame(matrix(0,ncol=9,nrow=samples))
						colnames(Dpred) <- c('X0', 'Dp1tr', 'Dp1', 'Dpe1', 'Dp2tr', 'DpSup2tr', 'DpAdtr', 'DpAd', 'DpeAd')

						#if(k==1){Dpred$X0 <- X0pred}
						if(k==2){Dpred$X0 <- X0predl}
						if(k==3){Dpred$X0 <- X0predm}
						if(k==4){Dpred$X0 <- X0predh}

						Dpred[,'Dp1tr'] <- exp(-(Nm-dmonthAE)*d0)*Dpred[,'X0']/(1+(g0/d0*(1-exp(-(Nm-dmonthAE)*d0))*Dpred[,'X0']))
						Dpred[,'Dp1'] <- Dpred[,'Dp1tr']*exp(-dmonthAE*d1)/(1+(G1/d1*(1-exp(-dmonthAE*d1))*Dpred[,'Dp1tr']))
						Dpred[,'Dpe1'] <- Dpred[,'Dp1']*exp(rnorm(n=1, mean=0,sd=Std1))
						Dpred[,'Dp2tr'] <- exp(-(Nm-dmonthAE)*d1)*Dpred[,'Dp1']/(1+(G1/d1*(1-exp(-(Nm-dmonthAE)*d1))*Dpred[,'Dp1']))

						# Adult estimates are initialized (initial arrivals of 2+ only, then use of successive cohorts (use of the first 20 lines to approach a DAd at equilibrium)
						Dpred[1,'DpAdtr'] <- Dpred[1,'Dp2tr']
						Dpred[1,'DpAd'] <- Dpred[1,'DpAdtr']*exp(-dmonthAE*DAd[1])/(1+(GAd[1]/DAd[1]*(1-exp(-dmonthAE*DAd[1]))*Dpred[1,'DpAdtr']))

						#### Vectorization seems impossible for this part:
						# The adult densities of the previous simulation (approximated as a 'previous year') are used to arrive after some simulation at a level of a population at equilibrium
						# Working by vectorizing does not involve DDep between adults in a way
						for (ii in 2:samples){
							Dpred[ii,'DpSup2tr'] <- exp(-(Nm-dmonthAE)*DAd[ii])*Dpred[ii-1,'DpAd']/(1+(GAd[ii]/DAd[ii]*(1-exp(-(Nm-dmonthAE)*DAd[ii]))*Dpred[ii-1,'DpAd']))
							Dpred[ii,'DpAdtr'] <- Dpred[ii,'Dp2tr'] + Dpred[ii,'DpSup2tr']
							Dpred[ii,'DpAd'] <- Dpred[ii,'DpAdtr']*exp(-dmonthAE*DAd[ii])/(1+(GAd[ii]/DAd[ii]*(1-exp(-dmonthAE*DAd[ii]))*Dpred[ii,'DpAdtr']))
							Dpred[ii,'DpeAd'] <- Dpred[ii,'DpAd']*exp(rnorm(n=1, mean=0, sd=StdAd[ii]))	# Estimated StdAd for error on sum of 2+ and> 2+ -> Overestimated uncertainty envelope on this visualization
						}

						# Saving the distributions
						#if(k==1){Dpredall <- Dpred}
						if(k==2){FD[[par_name]]$D1l <- hist(sapply(Dpred$Dpe1, function(x){min(max(SEQ0),max(min(SEQ0),x))}),breaks=SEQ0,plot=FALSE)$density; FD[[par_name]]$DAdl<-hist(sapply(Dpred[-c(1:10),'DpeAd'], function(x){min(max(SEQ0),max(min(SEQ0),x))}),breaks=SEQ0,plot=FALSE)$density}
						if(k==3){FD[[par_name]]$D1m <- hist(sapply(Dpred$Dpe1, function(x){min(max(SEQ0),max(min(SEQ0),x))}),breaks=SEQ0,plot=FALSE)$density; FD[[par_name]]$DAdm<-hist(sapply(Dpred[-c(1:10),'DpeAd'], function(x){min(max(SEQ0),max(min(SEQ0),x))}),breaks=SEQ0,plot=FALSE)$density}
						if(k==4){FD[[par_name]]$D1h <- hist(sapply(Dpred$Dpe1, function(x){min(max(SEQ0),max(min(SEQ0),x))}),breaks=SEQ0,plot=FALSE)$density; FD[[par_name]]$DAdh<-hist(sapply(Dpred[-c(1:10),'DpeAd'], function(x){min(max(SEQ0),max(min(SEQ0),x))}),breaks=SEQ0,plot=FALSE)$density}
						}

					## Moving average to smooth the curve
					if (disc>20){
						FD[[par_name]]=data.frame(apply(FD[[par_name]], 2, movingAverage, n=ceiling(0.05*disc)))
						}

					#save(FS, file='data/FS.RData')
					#save(FD, file='data/FD.RData')
					
				} # End of if condition (check if data already in existing FS)
			
			# Updating progress bar
			nrun=nrun+1
			setTxtProgressBar(pb, nrun)
			
			} # End of for loop (cache)
		} # End of for loop (t_90)
	} # End of for loop (t_10)
	
	save(FS, file=paste('data/FS',name_suff,'.RData', sep=''))
	save(FD, file=paste('data/FD',name_suff,'.RData', sep=''))
	close(pb)
	}

	

# All ranges
#t_10_L = seq(from=T10rg[1], to=T10rg[2], by=T10rg[3])
t_10_L = c(12)

t_90_L = seq(from=T90rg[1], to=T90rg[2], by=T90rg[3])
cache_L= seq(from=Crg[1], to=Crg[2], by=Crg[3])

generation_data(disc, X0, X1, XAd, t_10_L, t_90_L, cache_L, Nm, existing_FS_data_path=NULL, name_suff='_T103')




# Merging two FS & two FD and saving merged objects
	# existing_FS_data_path_1 : path to the first FS [path to the first FD is inferred]
	# existing_FS_data_path_2 : path to the second FS [path to the 2nd FD is inferred]
	# name_suff : string to paste at the end of 'FS' and 'FD' when saving the RData (output will be saved as 'FS<name_suff>.RData')
merge_data <- function(existing_FS_data_path_1, existing_FS_data_path_2, name_suff=''){
	# If existing data is used, we check that x-axis are the same
	load(file=existing_FS_data_path_1)
	load(file=gsub("FS", "FD", existing_FS_data_path_1)) # Path to FD is inferred from FS path.
	FS1=FS
	FD1=FD
	
	load(file=existing_FS_data_path_2)
	load(file=gsub("FS", "FD", existing_FS_data_path_2)) # Path to FD is inferred from FS path.
	
	# Progressbar initialization
	Nx=length(names(FS1))+length(names(FD1))

	# Check if x-axis are equal
	if (sum(FS1[["X0"]]==FS[["X0"]])!=length(FS[["X0"]])){
		stop('Loaded data (',existing_FS_data_path_1,' and ',existing_FS_data_path_2,') does not have the same x-axis !')
		}

	pb <- txtProgressBar(min=0, max=Nx, initial=0, title='Merging data', label='Merging data', style=3)
	nrun=0
	
	# Checking if data are available in FS et FD
	for (n in names(FS1)){
		if (!n %in% names(FS)){
			FS[[n]]=FS1[[n]]
			}
		# Updating progress bar
		nrun=nrun+1
		setTxtProgressBar(pb, nrun)
		}
	for (n in names(FD1)){
		if (!n %in% names(FD)){
			FD[[n]]=FD1[[n]]
			}
		# Updating progress bar
		nrun=nrun+1
		setTxtProgressBar(pb, nrun)
		}
		
	
	save(FS, file=paste('data/FS',name_suff,'.RData', sep=''))
	save(FD, file=paste('data/FD',name_suff,'.RData', sep=''))
	close(pb)
	}

	

# Merging with previously built data
#merge_data('data/FS_testT10_2.RData','data/FS_testT10_3.RData', name_suff='_testT10_4')

	