popPredTab <- function(pop.w, main.win, wpp.year) {
	ePopPred <- new.env()
	ePopPred$wpp.year <- wpp.year
	
	create.sim.dir.widget(env=ePopPred, parent=pop.w, type='pop', 
				main.win=main.win,
				default=eval(formals(pop.predict)$output.dir),
				no.mcmc=TRUE)
				
	nb <- gnotebook(cont=pop.w, expand=TRUE)
	
	# Predictions group
	pred.g <- ggroup(label="<span color='#B40404'>Make predictions</span>", markup=TRUE,
							horizontal=FALSE, cont=nb, spacing=10)
	pred.g.env <- popNewPred.group(pred.g, main.win, parent=ePopPred)
	
	# Result group
	result.g <- ggroup(label="<span color='#B40404'>Explore results</span>", markup=TRUE,
							horizontal=FALSE, cont=nb)
	result.g.env <- popResults.group(result.g, main.win, parent=ePopPred)
	
	svalue(nb) <- 1
	label <- glabel(paste('Dependency in use: bayesPop  v.', installed.packages()["bayesPop", "Version"]), cont=pop.w)
	font(label) <- c(style='italic', family='serif')

	
}