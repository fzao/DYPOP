#########################################
### Fonction de creation de la FIGURE 2 : RELATION type 'SR' : densité d'une classe d'âge l'année y en fonction des densités de la classe d'âge précédente l'année y-1
#### Integration d'un niveau de densité 1+ ou Ad influençant la survie
#########################################
	# ids : nom de la station (utilisé pour le titre)
	# FS : Données chargée (correspondant au set de paramètres en entrée)
	# T10, T90, C : Valeur des paramètres d'entrées du modèle (caractéristiques de la station T10 annuel, T90 annuel et % de S de caches)
	# X1m, XAdm : Valeur de 
	# [temp] OS : type d'OS (adapte les commandes d'ouverture de fenetres graphique
	
fig2_p0 <- function(ids, FS, t_10, t_90, cache)
{
	par_name=paste('T10',t_10,'T90',t_90,'C',cache, sep="_")
	#par_name_full=paste('T10',t_10,'T90',t_90,'C',cache,'X1m',X1m,'XAdm',XAdm, sep="_")
	disc=length(FS[["X0"]])
	
	
	# Mise en forme donnees
	x0 <- FS[["X0"]]
	s0_025 <- FS[[par_name]][,'r1_025']
	s0_25 <- FS[[par_name]][,'r1_25']
	s0_50 <- FS[[par_name]][,'r1_50']
	s0_75 <- FS[[par_name]][,'r1_75']
	s0_975 <- FS[[par_name]][,'r1_975']
	
	x1 <- FS[["X1"]]

	dataF <- data.frame(x0, s0_025,s0_25,s0_50,s0_75,s0_975)
		
	## Subplots
	p0 <- plot_ly(dataF, x = ~x0, y = ~s0_975, type = 'scatter', mode = 'lines',
			line = list(color = 'black'),
			showlegend = FALSE, name = 'Percentile 97.5') %>%
	  add_trace(y = ~ s0_025, type = 'scatter', mode = 'lines',
				fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'black'),
				showlegend = FALSE, name = 'Percentile 2.5') %>%
	  add_trace(y = ~ s0_25, type = 'scatter', mode = 'lines',
				line = list(color='black'),
				showlegend = FALSE, name = 'Percentile 25') %>%
	  add_trace(y = ~ s0_75, type = 'scatter', mode = 'lines',
				fill = 'tonexty', fillcolor='rgba(50,50,50,0.2)', line = list(color = 'black'),
				showlegend = FALSE, name = 'Percentile 75') %>%
	  add_trace(y = ~ s0_50, type = 'scatter', mode = 'lines',
				line = list(color='red'),
				showlegend = FALSE, name = 'Percentile 25') %>%
	  layout(title = 'Expected densities of 1+',
			 paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
			 xaxis = list(title = "D[0+, y-1]",
						  gridcolor = 'rgb(255,255,255)',
						  showgrid = TRUE,
						  showline = FALSE,
						  showticklabels = TRUE,
						  tickcolor = 'rgb(127,127,127)',
						  ticks = 'outside',
						  zeroline = FALSE,
						  range=c(min(x0),max(x0))),
			 yaxis = list(title = "D[1+, y]",
						  gridcolor = 'rgb(255,255,255)',
						  showgrid = TRUE,
						  showline = FALSE,
						  showticklabels = TRUE,
						  tickcolor = 'rgb(127,127,127)',
						  ticks = 'outside',
						  zeroline = FALSE,
						  range=c(min(x1),max(x1))))

	
	return(p0)
}


# type = "heatmap" or "surface"

fig2_hm <- function(ids, FS, t_10, t_90, cache, X1m, XAdm, type3D=FALSE)
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
		
	# Values of heatmap
	mat_Zm=c(c(0,0), c(max(as.matrix(FS[[paste(par_name,'_2D',sep='')]])), max(as.matrix(FS[[paste(par_name,'_2D',sep='')]]))))
	dim(mat_Zm)=c(2,2)
	
	HM <- plot_ly(z = as.matrix(FS[[paste(par_name,'_2D',sep='')]]),
			x = colnames(as.matrix(FS[[paste(par_name,'_2D',sep='')]])),
			y = rownames(as.matrix(FS[[paste(par_name,'_2D',sep='')]])),
			colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)")),
			cauto = F, cmin = 0, cmax = 40,
			type = 'heatmap', colorbar=list(title='D[>1+, y]'))
	
	if(type3D==TRUE){
	  HM <- HM %>%
	    add_surface(z = ~mat_Zm,
	                x=c(XAdm,XAdm), y=c(0,max(FS[['X1']])), opacity = 0.7, colorscale = list(c(0,0),c("rgb(255,112,184)","rgb(255,112,184)")),showscale = FALSE)%>%
	    add_surface(z = ~t(mat_Zm),
	                y=c(X1m,X1m), x=c(0,max(FS[['XAd']])), opacity = 0.7, colorscale = list(c(0,0),c("rgb(255,112,184)","rgb(255,112,184)")), showscale = FALSE)%>%
	    layout(scene = list(
	      xaxis=list(title = "D[>1+, y-1]"),
	      yaxis=list(title = "D[1+, y-1]"),
	      zaxis=list(title = "D[>1+, y]")))
	  }
	
	if(type3D==FALSE){
	  HM <- HM %>%
	    add_segments(x = XAdm, xend = XAdm, y = -100, yend = 1000, line = list(color = 'red'))%>%
	    add_segments(x = -100, xend = 1000, y = X1m, yend = X1m, line = list(color = 'orange'))%>%
	    layout(title = 'Expected densities of >1+',
	           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(239,239,239)',
	           #legend = list(orientation = 'h'),
	           legend = list(x = 0, y=-0.1),
	           xaxis = list(title = "D[>1+, y-1]",
	                        gridcolor = 'rgb(255,255,255)',
	                        showgrid = TRUE,
	                        showline = FALSE,
	                        showticklabels = TRUE,
	                        tickcolor = 'rgb(127,127,127)',
	                        ticks = 'outside',
	                        zeroline = FALSE,
	                        range=c(min(FS[["XAd"]]),max(FS[["XAd"]]))),
	           yaxis = list(title = "D[1+, y-1]",
	                        gridcolor = 'rgb(255,255,255)',
	                        showgrid = TRUE,
	                        showline = FALSE,
	                        showticklabels = TRUE,
	                        tickcolor = 'rgb(127,127,127)',
	                        ticks = 'outside',
	                        zeroline = FALSE,
	                        range=c(min(FS[["X1"]]),max(FS[["X1"]]))),
	           showlegend = FALSE)
	  }
	

	
	# Marginal survival 1+ -> Ad (right margin : Rotated)
	p1 <- plot_ly(dataF, x = ~s1_975, y = ~x1, type = 'scatter', mode = 'lines',
			line = list(color = 'black'),
			showlegend = FALSE, name = 'Percentile 97.5') %>%
	  add_trace(x = ~ s1_025, type = 'scatter', mode = 'lines',
				fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'black'),
				showlegend = FALSE, name = 'Percentile 2.5') %>%
	  add_trace(x = ~ s1_25, type = 'scatter', mode = 'lines',
				line = list(color='black'),
				showlegend = FALSE, name = 'Percentile 25') %>%
	  add_trace(x = ~ s1_75, type = 'scatter', mode = 'lines',
				fill = 'tonexty', fillcolor='rgba(50,50,50,0.2)', line = list(color = 'black'),
				showlegend = FALSE, name = 'Percentile 75') %>%
	  add_trace(x = ~ s1_50, type = 'scatter', mode = 'lines',
				line = list(color='red'),
				showlegend = FALSE, name = 'Percentile 25')%>%
	  layout(showlegend = FALSE, plot_bordercolor='red', xaxis = list(title = paste("D[>1+, y]")), xaxis = list(range=c(min(FS[["X1"]]),max(FS[["X1"]]))))


	pAd <- plot_ly(dataF, x = ~xAd, y = ~sAd_975, type = 'scatter', mode = 'lines',
			line = list(color = 'black'),
			showlegend = FALSE, name = 'Percentile 97.5') %>%
	  add_trace(y = ~ sAd_025, type = 'scatter', mode = 'lines',
				fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'black'),
				showlegend = FALSE, name = 'Percentile 2.5') %>%
	  add_trace(y = ~ sAd_25, type = 'scatter', mode = 'lines',
				line = list(color='black'),
				showlegend = FALSE, name = 'Percentile 25') %>%
	  add_trace(y = ~ sAd_75, type = 'scatter', mode = 'lines',
				fill = 'tonexty', fillcolor='rgba(50,50,50,0.2)', line = list(color = 'black'),
				showlegend = FALSE, name = 'Percentile 75') %>%
	  add_trace(y = ~ sAd_50, type = 'scatter', mode = 'lines',
				line = list(color='red'),
				showlegend = FALSE, name = 'Percentile 25')%>%
	  layout(showlegend = FALSE, plot_bordercolor='orange', yaxis = list(title = paste("D[>1+, y]")), yaxis = list(range=c(min(FS[["XAd"]]),max(FS[["XAd"]]))))

				
	# Heat map combined with marginal views
	HMcomb <- subplot(pAd, plotly_empty(), HM, p1, nrows = 2, shareX=TRUE, shareY=TRUE, heights = c(0.2, 0.8), widths = c(0.8,0.2))
	
	
	return(HMcomb)
	}

