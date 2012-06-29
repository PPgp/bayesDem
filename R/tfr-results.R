TFRresults.group <- function(g, main.win, parent) {
	e <- new.env()
	
	#e$country.table <- get.table.of.countries(file.path(.find.package("bayesTFR"), "data", 
	#						paste('WPP', parent$wpp.year, '_LOCATIONS', '.csv', sep='')))
							
	e$sim.dir <- parent$sim.dir
	graph.defaults <- formals(png)
	mcmc.defaults <- formals(run.tfr.mcmc)
	nb <- bDem.gnotebook(container=g, expand=TRUE)
	show.traj.g <- ggroup(label="<span color='#0B6138'>TFR trajectories</span>", 
							markup=TRUE, horizontal=FALSE, container=nb)
	create.trajectories.group(show.traj.g, e, main.win)	
	
	map.g <- ggroup(label="<span color='#0B6138'>TFR world maps</span>", markup=TRUE, 
					horizontal=FALSE, container=nb)
	create.maps.group(map.g, e, main.win)

	dl.curve.g <- ggroup(label="<span color='#0B6138'>DL curve</span>", markup=TRUE, 
							horizontal=FALSE, container=nb)
	create.dlcurves.group(dl.curve.g, e, main.win)
	
	traces.g <- ggroup(label="<span color='#0B6138'>Parameter traces</span>", markup=TRUE, horizontal=FALSE, container=nb)
	create.partraces.group(traces.g, e, main.win)
	
	############################################
	# Convergence Diagnostics
	############################################
	convergence.g <- ggroup(label="<span color='#0B6138'>Convergence</span>", markup=TRUE, horizontal=FALSE, container=nb)
	create.convergence.tab(convergence.g, e$sim.dir, main.win=main.win)	
	svalue(nb) <- 1
	return(e)
}

.create.trajectories.settings.group <- function(g, e, defaults, l=1) {
	leftcenter <- c(-1,0)
	show.traj.tfr.f <- gframe("<span color='blue'>Trajectories settings</span>", markup=TRUE, 
								horizontal=FALSE, container=g)
	lo <- glayout(container=show.traj.tfr.f) 
	lo[l,1, anchor=leftcenter] <- glabel('CI (%):', container=lo)
	lo[l,2] <- e$pi <- gedit('80, 95', width=7, container=lo)
	lo[l+1,1, anchor=leftcenter] <- glabel('# trajectories:', container=lo)
	lo[l+1,2] <- e$nr.traj <- gedit(20, width=6, container=lo)
	lo[l,3, anchor=leftcenter] <- 	glabel('From year:', container=lo)
	lo[l,4] <- e$start.year <- gedit(defaults$start.year, width=4, container=lo)
	lo[l+1,3, anchor=leftcenter] <- glabel('To year:', container=lo)
	lo[l+1,4] <- e$end.year <- gedit(defaults$end.year, width=4, container=lo)
	lo[l,5] <- glabel('     ', container=lo)
	lo[l,6:7] <- e$half.child.variant <- gcheckbox('+/- 0.5 child', checked=defaults$half.child.variant, 
								container=lo)
	lo[l+1,6:7] <- e$typical.trajectory <- gcheckbox('Typical trajectory', checked=defaults$typical.trajectory, 
								container=lo)
	return(lo)
}
#################################################################################

create.trajectories.group <- function(g, e, main.win) {
	############################################
	# TFR Trajectories
	############################################
	defaults.pred <- formals(tfr.predict)
	defaults.traj <- formals(tfr.trajectories.plot)
	defaults.traj.all <- formals(tfr.trajectories.plot.all)
	mcmc.defaults <- formals(run.tfr.mcmc)
	
	addSpace(g, 10)
	show.traj.country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$show.traj.country <- create.country.widget(show.traj.country.f, defaults.traj.all, 
									main.win, prediction=TRUE, parent.env=e)
		
	addSpace(g, 10)
	.create.trajectories.settings.group(g, e, defaults=list(start.year=mcmc.defaults$start.year, end.year=defaults.pred$end.year,
															half.child.variant=defaults.traj$half.child.variant,
															typical.trajectory=defaults.traj$typical.trajectory))
	addSpace(g, 10)
	
	show.traj.graph.f <- gframe("<span color='blue'>Advanced graph parameters</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	e$traj.graph.pars <- create.graph.pars.widgets(show.traj.graph.f, main.win=main.win)
	addSpring(g)
	show.traj.bg <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='tfr.trajectories.plot', package='bayesTFR', parent.group=show.traj.bg,
						parent.window=main.win)
	addSpring(show.traj.bg)
	create.generate.script.button(handler=showTFRtraj, action=list(mw=main.win, env=e, type='plot', script=TRUE),
								container=show.traj.bg)
	addSpace(show.traj.bg, 5)
	TableB.show.traj.act <- gaction(label='Table', icon='dataframe', handler=showTFRtraj, 
						action=list(mw=main.win, env=e, type='table', script=FALSE))
	GraphB.show.traj.act <- gaction(label='Graph', icon='lines', handler=showTFRtraj, 
						action=list(mw=main.win, env=e, type='plot', script=FALSE))
	e$TableB.show.traj <- bDem.gbutton(action=TableB.show.traj.act, container=show.traj.bg)
	bDem.gbutton(action=GraphB.show.traj.act, container=show.traj.bg)
}

.create.map.settings.group <- function(g, e) {
	leftcenter <- c(-1,0)
	map.set.f <- gframe("<span color='blue'>Map settings</span>", markup=TRUE, 
									horizontal=FALSE, container=g)
	mlo <- glayout(container=map.set.f)
	mlo[1,1, anchor = leftcenter] <- glabel('Percentile:', container=mlo)
	e$percentiles <- list('median'=0.5, 'lower 80'=0.1, 'upper 80'=0.9, 'lower 90'=0.05, 'upper 90'=0.95,
						'lower 95'=0.025, 'upper 95'=0.975, 'lower 60'=0.2, 'upper 60'=0.8,
						'lower 50'=0.25, 'upper 50'=0.75, 'lower 40'=0.3, 'upper 40'=0.7, 
						'lower 20'=0.4, 'upper 20'=0.6
						)
	mlo[1,2] <- e$map.percentile <- gdroplist(names(e$percentiles), container=mlo)
	mlo[1,3] <- '    ' # add some space between the two groups
	mlo[2,1, anchor = leftcenter] <- glabel('Bounds:    ', container=mlo)
	mlo[2,2] <- bounds.g <- ggroup(horizontal=TRUE, container=mlo)
	e$map.bounds <- gdroplist(c(80, 90, 95, 60, 50, 40, 20), container=bounds.g)
	glabel('%', container=bounds.g)	
	mlo[3,1, anchor = leftcenter] <- glabel('Measure:', container=mlo)
	mlo[3,2] <- e$map.measure <- gdroplist(c('TFR', 'lambda', bayesTFR:::tfr.parameter.names.cs.extended()), container=mlo)	
	mlo[1,4, anchor = leftcenter] <- glabel('Use R package:', container=mlo)
	mlo[1:2,5] <- e$map.package <- gradio(c('rworldmap', 'googleVis'), horizontal = FALSE, 
						handler=function(h, ...) {
							enabled(e$map.bounds) <- svalue(h$obj) == 'googleVis';
							enabled(e$map.same.scale) <- svalue(h$obj) == 'rworldmap'}, 
						container=mlo)
	mlo[3,4:5] <- e$map.same.scale <- gcheckbox('Same scale for all maps', checked=TRUE, container=mlo)
	enabled(e$map.bounds) <- svalue(e$map.package) == 'googleVis'
	enabled(e$map.same.scale) <- svalue(e$map.package) == 'rworldmap'
	return(mlo)
}

create.maps.group <- function(g, e, main.win) {
	############################################
	# TFR World Maps
	############################################
	addSpace(g, 10)
	.create.map.settings.group(g, e)
	addSpring(g)
	map.bg <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='tfr.map', package='bayesTFR', parent.group=map.bg,
						parent.window=main.win)
	addSpring(map.bg)
	create.generate.script.button(handler=showMap, action=list(mw=main.win, env=e, script=TRUE),
								container=map.bg)
	addSpace(map.bg, 5)
	GraphB.map <- gaction(label=' Show Map ', handler=showMap, 
						action=list(mw=main.win, env=e, script=FALSE))
	bDem.gbutton(action=GraphB.map, container=map.bg)
	addHandlerChanged(e$map.measure, 
					handler=function(h,...) enabled(e$map.same.scale) <- svalue(h$obj) == 'TFR')
}

create.dlcurves.group <- function(g, e, main.win) {
	leftcenter <- c(-1,0)
	############################################
	# DL Curves
	############################################
	defaults.dl <- formals(DLcurve.plot)
	defaults.dl.all <- formals(DLcurve.plot.all)
	addSpace(g, 10)
	dlc.country.f <- gframe("<span color='blue'>Country settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	e$dlc.country <- create.country.widget(dlc.country.f, defaults.dl.all, main.win, prediction=FALSE, 
											parent.env=e)
	addSpace(g, 10)
	dlc.dl.f <- gframe("<span color='blue'>DL curve settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	dllo <- glayout(horizontal=TRUE, container=dlc.dl.f)
	dllo[1,1, anchor=leftcenter] <- glabel('CI (%):', container=dllo)
	dllo[1,2] <- e$dlc.ci <- gedit('80, 95', width=7, container=dllo)
	dllo[1,3, anchor=leftcenter] <- glabel('Burnin:', container=dllo)
	dllo[1,4] <- e$dlc.bi <- gedit(defaults.dl$burnin, width=5, container=dllo)
	dllo[2,3, anchor=leftcenter] <- glabel('Maximum TFR:', container=dllo)
	dllo[2,4] <- e$dlc.tfr.max <- gedit(defaults.dl$tfr.max, width=2, container=dllo)
	dllo[2,1, anchor=leftcenter] <- glabel('# curves:', container=dllo)
	dllo[2,2] <- e$dlc.nr.curves <- gedit(20, width=6, container=dllo)
	
	dllo[1,5] <- e$predictive.distr <- gcheckbox('Predictive distribution', 
							checked=defaults.dl$predictive.distr, container=dllo)
	addSpace(g, 10)			
	dlc.graph.f <- gframe("<span color='blue'>Advanced graph parameters</span>", markup=TRUE, 
						horizontal=FALSE, container=g)
	e$dlc.graph.pars <- create.graph.pars.widgets(dlc.graph.f, main.win=main.win)
	addSpring(g)
	dlc.bg <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='DLcurve.plot', package='bayesTFR', parent.group=dlc.bg,
						parent.window=main.win)
	addSpring(dlc.bg)
	create.generate.script.button(handler=showDLcurve, action=list(mw=main.win, env=e, script=TRUE),
								container=dlc.bg)
	addSpace(dlc.bg, 5)
	GraphB.dlc <- gaction(label='Graph', icon='lines', handler=showDLcurve, 
						action=list(mw=main.win, env=e, script=FALSE))
	bDem.gbutton(action=GraphB.dlc, container=dlc.bg)
}

.create.partraces.settings.group <- function(g, e, par.names, par.names.cs) {
	leftcenter <- c(-1,0)
	addSpace(g, 10)	
	f <- gframe("<span color='blue'>Parameter traces settings</span>", markup=TRUE, 
							horizontal=FALSE, container=g)
	tlo <- glayout(container=f)
	tlo[1,1, anchor=leftcenter] <- glabel('Parameters:', container=tlo)
	tlo[1,2] <- e$traces.pars.chb <- gcheckbox("all", container=tlo, checked=TRUE, 
							handler=function(h,...) {
								if(svalue(e$traces.cs.chb, index=TRUE)==2) {
									enabled(e$traces.par.cs.dl)<-!svalue(h$obj)
									enabled(e$traces.par.dl)<-FALSE
								} else {
									enabled(e$traces.par.dl)<-!svalue(h$obj)
									enabled(e$traces.par.cs.dl)<-FALSE
									}})
	tlo[2:3,1:2] <- e$traces.cs.chb <- gradio(c('World parameters', 'Country specific'), horizontal = FALSE,
						container=tlo, 
						handler=function(h,...) {
								if (svalue(h$obj, index=TRUE)==2) {
									enabled(e$traces.par.cs.dl)<-!svalue(e$traces.pars.chb)
									enabled(e$traces.par.dl)<-FALSE
								} else {
									enabled(e$traces.par.dl)<-!svalue(e$traces.pars.chb)
									enabled(e$traces.par.cs.dl)<-FALSE
									}
								enabled(e$traces.country$country.w) <- svalue(h$obj, index=TRUE)==2
								enabled(e$traces.country$country.select.b) <- svalue(h$obj, index=TRUE)==2
								}
							)
	tlo[2,3:4] <- e$traces.par.dl <- gdroplist(par.names, container=tlo)
	enabled(e$traces.par.dl) <- FALSE
	tlo[3,3:4] <- e$traces.par.cs.dl <- gdroplist(par.names.cs, container=tlo)
	enabled(e$traces.par.cs.dl) <- FALSE
	tlo[4,1:4] <- cw <- ggroup(horizontal=TRUE, container=tlo)
	e$traces.country <- create.country.widget(cw,  main.win=main.win, show.all=FALSE, prediction=FALSE, 
											parent.env=e)
	enabled(e$traces.country$country.w) <- svalue(e$traces.cs.chb, index=TRUE)==2
	enabled(e$traces.country$country.select.b) <- svalue(e$traces.cs.chb, index=TRUE)==2
	tlo[1,5] <- '    '
	tlo[2,6, anchor=leftcenter] <- glabel('# points:', container=tlo)
	tlo[2,7] <- e$traces.nr.points <- gedit(100, width=5, container=tlo, coerce.with=as.numeric)
	tlo[3,6, anchor=leftcenter] <- glabel("Burnin:", container=tlo)
	tlo[3,7] <- e$traces.burnin <- gedit(0, width=5, container=tlo, coerce.with=as.numeric)
	tlo[4,6, anchor=leftcenter] <- glabel("Thin:", container=tlo)
	tlo[4,7] <- e$traces.thin <- gedit(0, width=5, container=tlo, coerce.with=as.numeric)
}

create.partraces.group <- function(g, e, main.win) {
	############################################
	# Parameter Traces
	############################################
	.create.partraces.settings.group(g, e, par.names=tfr.parameter.names(), par.names.cs=tfr.parameter.names.cs())
	addSpring(g)
	traces.bg <- ggroup(horizontal=TRUE, container=g)
	create.help.button(topic='tfr.partraces.plot', package='bayesTFR', parent.group=traces.bg,
						parent.window=main.win)	
	addSpring(traces.bg)
	SummaryB.traces <- gaction(label='Show summary', icon='dataframe', handler=showParTraces, 
						action=list(mw=main.win, env=e, print.summary=TRUE))
	bDem.gbutton(action=SummaryB.traces, container=traces.bg)
	GraphB.traces <- gaction(label='Graph', icon='lines', handler=showParTraces, 
						action=list(mw=main.win, env=e, print.summary=FALSE))
	bDem.gbutton(action=GraphB.traces, container=traces.bg)
}

create.convergence.tab <- function(parent, sim.dir, type='tfr', package='bayesTFR', main.win=NULL) {
	defaults <- formals(paste(type,'.diagnose', sep=''))
	e <- new.env()
	e$sim.dir <- sim.dir
	leftcenter <- c(-1,0)
	addSpace(parent, 10)
	#g <- ggroup(horizontal=FALSE, container=parent, expand=FALSE)
	g <- glayout(container=parent)
	#g1 <- ggroup(horizontal=TRUE, container=g, expand=TRUE)
	g[1,1] <- g2 <- gframe("<span color='blue'>Diagnostics settings</span>", markup=TRUE, horizontal=FALSE, container=g)
	mclo <- glayout(container=g2)
	mclo[1,1, anchor=leftcenter] <- glabel('Thin:', container=mclo)
	mclo[1,2] <- e$thin <- gedit(defaults$thin, width=5, container=mclo)
	mclo[2,1, anchor=leftcenter] <- glabel('Burnin:', container=mclo)
	mclo[2,2] <- e$burnin <- gedit(defaults$burnin, width=5, container=mclo)
	mclo[3,1:2] <- e$keep.thin.mcmc <- gcheckbox('Keep thinned MCMCs', checked = defaults$keep.thin.mcmc, container=mclo)
	#g3  <- gframe("<span color='blue'>Optional settings</span>", markup=TRUE, horizontal=FALSE, container=g1)
	#oplo <- glayout(container=g3)
	mclo[1,3] <- e$express <- gcheckbox('Express', checked=defaults$express, container=mclo,
						handler=function(h,...) enabled(e$country.sampling.prop) <- !svalue(h$obj))
	mclo[1,4] <- e$verbose <- gcheckbox('Verbose', checked = defaults$verbose, container=mclo)
	
	mclo[2,3, anchor=leftcenter] <- glabel('Proportion of countries included (0-1):', container=mclo)
	mclo[2,4] <- e$country.sampling.prop <- gedit(1, width=5, container=mclo)
	
	#addSpace(parent, 10)
	g[2, 1] <- '  '
	g[3,1] <- bDem.gbutton('    Show available convergence diagnostics    ', container=g, handler=showConvergenceDiag,
					action=list(mw=main.win, env=e, type=type), fill=TRUE)
	
	# This line is commented out because (for some reason) on Windows OS it causes the main window to go out of whack.
	#glo[l,2] <- (e$country.sampling.prop <- gslider(from=0, to=1, by=1/200, value=1, container=g4))
	enabled(e$country.sampling.prop) <- !defaults$express
	
	addSpring(parent)
	butg <- ggroup(horizontal=TRUE, container=parent)
	create.help.button(topic=paste(type,'.diagnose', sep=''), package=package, parent.group=butg,
						parent.window=main.win)	
	addSpring(butg)
	create.generate.script.button(handler=computeConvergenceDiag, 
					action=list(mw=main.win, env=e, type=type, package=package, script=TRUE),
								container=butg)
	addSpace(butg, 5)
	ComputeDiag <- gaction(label=' Compute New Diagnostics ', icon='execute', handler=computeConvergenceDiag, 
						action=list(mw=main.win, env=e, type=type, package=package, script=FALSE))
	bDem.gbutton(action=ComputeDiag, container=butg)	
}

create.country.widget <- function(parent, defaults=NULL, main.win=NULL, show.all=TRUE, 
									prediction=FALSE, parent.env=NULL, disable.table.button=TRUE) {
	e <- new.env()
	e$parent.env <- parent.env
	e$prediction <- prediction
	leftcenter <- c(-1, 0)
	rightcenter <- c(1, 0)
	#g1 <- ggroup(horizontal=TRUE, container=parent)
	g1 <- glayout(container=parent)
	g1[1,1, anchor=leftcenter] <- glabel('Country:', container=g1)
	#if(show.all) {glabel("<span color='red'>(*)</span>", markup=TRUE, container=g1)}
	#else glabel("<span color='red'>*</span>", markup=TRUE, container=g1)
	g1[1,2] <- e$country.w <- gedit(width=20, container=g1)
	g1[1,3] <- e$country.select.b <- bDem.gbutton('Select', container=g1, handler=selectCountryMenu,
								action=list(mw=main.win, text.widget=e$country.w, env=e))
	if(show.all) {
		g1[2,1:3] <- gseparator(container=g1)
		#g2 <- ggroup(horizontal=TRUE, container=parent)
		g1[3,1] <- e$all.countries.chb <- gcheckbox('All countries', checked=FALSE, container=g1,
									handler=function(h,...){
										enabled(e$country.w) <- !svalue(h$obj)
										enabled(e$country.select.b) <- !svalue(h$obj)
										enabled(e$all.type) <- svalue(h$obj)
										enabled(e$all.output) <- svalue(h$obj)
										if(disable.table.button) enabled(parent.env$TableB.show.traj) <- !svalue(h$obj)
										})
		#addSpace(g2, 15)
		g1[3,2, anchor=rightcenter] <- glabel("Output type:", container=g1)
		g1[3,3] <- e$all.type <- gdroplist(c('png', 'jpeg', 'pdf', 'tiff', 'bmp', 'postscript'), container=g1)
		enabled(e$all.type) <- FALSE
		#g3 <- ggroup(horizontal=TRUE, container=parent)
		#addSpace(g3,7)
		g1[4,1, anchor=leftcenter] <- glabel("Output directory:", container=g1)
		g1[4,2:3] <- e$all.output <- bDem.gfilebrowse(eval(defaults$output.dir), type='selectdir', 
					  width=39, quote=FALSE, container=g1)
		enabled(e$all.output) <- FALSE
	}
	return(e)	
}



create.graph.pars.widgets <- function (parent, main.win=NULL) {
	g <- ggroup(horizontal=FALSE, container=parent)
	glabel("Comma-separated parameters in R format (see '?par'), e.g. ylim=c(0,6), xlab='Time'", container=g)
	pars.w <- gedit('', width=40, container=g)
	return(pars.w)
}

get.table.of.countries.from.meta <- function(sim.dir, prediction=FALSE, sorted=TRUE, 
										pred.type='tfr', env=NULL) {
	if(!is.null(env$prior.select.countries.function)) # function to run prior the selection. Can be used to set something to the env.
		do.call(env$prior.select.countries.function, list(env))
	if(prediction) {
		pred.call <- paste('get.', pred.type, '.prediction', sep='')
		args <- formals(pred.call)
		args$sim.dir <- NULL
		lenv <- as.list(env)
		add.args <- lenv[names(args)[!sapply(lenv[names(args)], is.null)]]
		pred <- do.call(pred.call, c(list(sim.dir=sim.dir), add.args))
		if(is.null(pred)) {
			gmessage('Simulation directory contains no valid predictions.', 
					title='Input Error', icon='error')
			return(NULL)
		}
		loc.data <- get.countries.table(pred)
	} else { #simulation
		mcmc.set <- do.call(paste('get.', pred.type, '.mcmc', sep=''), list(sim.dir=sim.dir))
		if(is.null(mcmc.set)) {
			gmessage('Simulation directory contains no valid MCMC results.', title='Input Error',
					icon='error')
			return(NULL)
		}
		loc.data <- get.countries.table(mcmc.set)	}
	if(sorted) {
		ord.idx <- order(loc.data[,'name'])
		loc.data <- loc.data[ord.idx,]
	}
	return(loc.data)
}

draw.new.country.select <- function(used, env) {
	for(item in c('sim.dir', env$new.country.select.if.changed))
		if (svalue(env[[item]]) != used[[item]]) return (TRUE)
	return(FALSE)
}
	
set.used.items <- function(env.used, env) {
	for(item in c('sim.dir', env$new.country.select.if.changed))
		env.used[[item]] <- svalue(env[[item]])
}

selectCountryMenu <- function(h, ...) {
	country.selected <- function(h1, ...) {
		selected.country <- as.numeric(svalue(h$action$env$selcountry.gt))
		selected.country <- get.country.object(selected.country, 
								country.table=h$action$env$country.table)
		if (length(selected.country) > 0) {
			svalue(h$action$text.widget) <- selected.country$name
		} 
		visible(h$action$env$country.sel.win) <- FALSE
	}
	new.window <- TRUE
	if (!is.null(h$action$env$country.sel.win)) {
		# if anything has changed (sim.dir or the data), the window needs to be re-built
		if(draw.new.country.select(h$action$env$used, h$action$env$parent.env)) {
			dispose(h$action$env$country.sel.win)
			new.window <- TRUE
		} else {
			country.table <- get.table.of.countries.from.meta(svalue(h$action$env$parent.env$sim.dir), 
								prediction=h$action$env$prediction, 
								pred.type=if(is.null(h$action$env$parent.env$pred.type)) 'tfr' 
											else h$action$env$parent.env$pred.type, 
								env=h$action$env$parent.env)
			if(is.null(country.table)) {
				dispose(h$action$env$country.sel.win)
				return(NULL)
			}
			if(dim(country.table)[1] != dim(h$action$env$country.table)[1]) {
				dispose(h$action$env$country.sel.win)
				new.window <- TRUE
			} else {
				new.window <- FALSE
				visible(h$action$env$country.sel.win) <- TRUE
			}
		}
	}
	if(new.window) {
		sim.dir.used <- svalue(h$action$env$parent.env$sim.dir)
		country.table <- get.table.of.countries.from.meta(sim.dir.used, prediction=h$action$env$prediction,
										pred.type=if(is.null(h$action$env$parent.env$pred.type)) 'tfr' 
											else h$action$env$parent.env$pred.type, 
										env=h$action$env$parent.env)
		if (is.null(country.table)) return(NULL)
		h$action$env$used <- new.env()
		set.used.items(h$action$env$used, h$action$env$parent.env)
		h$action$env$country.table <- country.table
		h$action$env$country.sel.win <- win <- gwindow('Select country', parent=h$action$mw, height=450,
					handler=function(h, ...) {
						h$action$env$country.sel.win<-NULL;
						h$action$env$selcountry.ok.handler <- NULL;
						h$action$env$selcountry.gt.handler <- NULL
					},
					action=list(env=h$action$env))
		t.group <- ggroup(horizontal=FALSE, container=win)
		h$action$env$selcountry.gt <- gtable(h$action$env$country.table, container=t.group, expand=TRUE,				handler=country.selected)
		b.group <- ggroup(horizontal=TRUE, container=t.group)
		gbutton('Cancel', container=b.group, handler=function(h, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		h$action$env$selcountry.okbutton <- gbutton('OK', container=b.group)
	}
	if(!is.null(h$action$env$selcountry.ok.handler)) 
		removehandler(h$action$env$selcountry.okbutton, h$action$env$selcountry.ok.handler)
	h$action$env$selcountry.ok.handler <- addhandlerclicked(h$action$env$selcountry.okbutton, 
												handler=country.selected)
	if(!is.null(h$action$env$selcountry.gt.handler)) 
		removehandler(h$action$env$selcountry.gt, h$action$env$selcountry.gt.handler)
	h$action$env$selcountry.gt.handler <- addhandlerdoubleclick(h$action$env$selcountry.gt, 
												handler=country.selected)
}
	


get.country.code.from.widget <- function(country.widget, env, force.country.spec=FALSE) {
	country <- svalue(country.widget)
	country.selected <- TRUE
	if (!is.null(env$all.countries.chb)) { 
		if(svalue(env$all.countries.chb)) country.selected <- FALSE
	}
	if (force.country.spec) country.selected <- TRUE
	if (country.selected) {
		if (nchar(country)==0) {
			gmessage('Country must be specified.', title='Input Error',
					icon='error')
			return(NULL)
		}
		warn <- getOption('warn')
		options(warn=-1)
		country.code <- as.numeric(country)
		if (!is.na(country.code)) country <- country.code
		options(warn=warn)
		country <- get.country.object(country, country.table=env$country.table)
		if(is.null(country$name)) {
			gmessage('Country does not exist.', title='Input Error',
					icon='error')
			return(NULL)
		}
		return(country)
	}
	return(list(code=NULL, output.dir=svalue(env$all.output), output.type=svalue(env$all.type)))
}
	
showTFRtraj <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	show.type <- h$action$type
	country.pars <- get.country.code.from.widget(e$show.traj.country$country.w, e$show.traj.country, 
							force.country.spec=show.type!='plot')
	if(is.null(country.pars)) return(NULL)
	param.names1 <- list(text='sim.dir', numeric=c('nr.traj', 'start.year', 'end.year'),
							logical=c('half.child.variant', 'typical.trajectory'))
	param.env <- get.parameters(param.names1, env=e, quote=h$action$script)
	param.env[['pi']] <- svalue(e$pi) # because this is a vector and we want to keep this as a string for later parsing
	param.env.rest <- list(country=country.pars$code, output.dir=country.pars$output.dir,
							output.type=country.pars$output.type, verbose=TRUE)
	param.env <- c(param.env, 
					get.parameters(list(text=c('output.dir', 'output.type'), 
										logical='verbose', numeric='country'), env=param.env.rest, 
										quote=h$action$script, retrieve.from.widgets=FALSE))
	param.pred <- param.env['sim.dir']
	# get it now unquoted (to avoid double quotes if script is TRUE)
	param.pred.ev <- get.parameters(list(text='sim.dir'), env=e, quote=FALSE)
	pred <- do.call('get.tfr.prediction', param.pred.ev)
	if(h$action$script) {
		cmd <- paste('pred <- get.tfr.prediction(', paste(paste(names(param.pred), param.pred, sep='='), collapse=', '), 
						')\n', sep='')
	} else {	
		cmd <- ''
	}
	xmin <- param.env[['start.year']]
	xmax <- param.env[['end.year']]
	if(nchar(xmin) > 0 | nchar(xmax) > 0) {
		param.env[['xlim']] <- paste(if(nchar(xmin)>0) xmin else pred$mcmc.set$meta$start.year,
									 if(nchar(xmax)>0) xmax else pred$end.year, sep=', ')	}
	param.names.graph <- list(numvector=c('pi', 'xlim'), numeric=c('country', 'nr.traj'), 
							logical=c('half.child.variant', 'typical.trajectory'))
	param.names.table <- list(numvector='pi', numeric='country', logical='half.child.variant')

	pars.value <- svalue(e$traj.graph.pars)
	if (show.type == 'plot') {
		if(!is.null(country.pars$code)) { # one country
			param.plot1c <- get.parameters(param.names.graph, env=param.env, quote=h$action$script, 
											retrieve.from.widgets=FALSE)
			cmd <- paste(cmd, 'tfr.trajectories.plot(pred,',
						paste(paste(names(param.plot1c), param.plot1c, sep='='), collapse=', '), ',',
						pars.value, ')')
			if (h$action$script) {
				create.script.widget(cmd, h$action$mw, package="bayesTFR")
			} else {
				create.graphics.window(parent=h$action$mw, title=paste("Trajectories for", country.pars$name))
				eval(parse(text=cmd))
			}
		} else { # all countries
			param.names.graph[['text']] <- c('output.dir', 'output.type')
			param.names.graph$logical <- c(param.names.graph$logical, 'verbose')
			param.plot.allc <- get.parameters(param.names.graph, env=param.env, quote=h$action$script,
												 retrieve.from.widgets=FALSE)
			cmd <- paste(cmd, 'tfr.trajectories.plot.all(pred, ', 
						paste(paste(names(param.plot.allc), param.plot.allc, sep='='), collapse=', '), sep='')
			if(!is.null(pars.value)) {
				if(nchar(pars.value)>0)
					cmd <- paste(cmd, ',', pars.value)
			}
			cmd <- paste(cmd, ')', sep='')
			if (h$action$script) {
				create.script.widget(cmd, h$action$mw, package="bayesTFR")
			} else {
				eval(parse(text=cmd))
			}
		}
	} else {
		# Table
		param.table <- get.parameters(param.names.table, env=param.env, quote=FALSE, retrieve.from.widgets=FALSE)
		table.values <- do.call('tfr.trajectories.table', c(list(tfr.pred=pred), param.table))
		table.values <- round(table.values[!apply(is.na(table.values), 1, all),],2)
		table.values <- cbind(rownames(table.values), table.values)
		colnames(table.values)[1] <- 'year'
		win <- gwindow(country.pars$name, parent=h$action$mw, height=max(min(22.2*(dim(table.values)[1]+1),600), 100))
		g <- ggroup(container=win, horizontal=FALSE, expand=TRUE)
		gt <- gtable(table.values, container=g, expand=TRUE)
		gbutton('Print to R Console', container=g, handler=function(h,...){
										print(do.call('tfr.trajectories.table', c(list(tfr.pred=pred), param.table)))})
	}
}

showMap <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	percentile <- svalue(e$map.percentile)
	quantile <- e$percentiles[[percentile]]
	param.env <-list(sim.dir=svalue(e$sim.dir), quantile=quantile)
	param.names1 <- list(text='sim.dir')
	param.pred <- get.parameters(param.names1, env=param.env, quote=h$action$script, retrieve.from.widgets=FALSE)
	same.scale <- svalue(e$map.same.scale)
	par.name <- svalue(e$map.measure)
	bounds <- svalue(e$map.bounds)
	package <- svalue(e$map.package)
	map.function <- if(package == 'rworldmap') 'tfr.map' else 'tfr.map.gvis'
	if(h$action$script) {
		cmd <- paste('pred <- get.tfr.prediction(', paste(paste(names(param.pred), param.pred, sep='='), collapse=', '), 
						')\n', sep='')
		if (par.name == 'TFR') {
			 if(package == 'rworldmap') {
				cmd <- paste(cmd, "param.map <- get.tfr.map.parameters(pred, same.scale=", same.scale,
					", quantile=", quantile, ")\n", sep="")
				cmd <- paste(cmd, 'do.call("', map.function, '", param.map)', sep='')
			} else {
				cmd <- paste(cmd, map.function, '(pred, quantile=', quantile, ', pi=', bounds, ')', sep='')
			}
		} else {
			cmd <- paste(cmd, map.function, '(pred, quantile=', quantile, ', par.name="', par.name, '"', sep='')
			cmd <- paste(cmd, if (package == 'googleVis') paste(', pi=', bounds, sep='') else '', sep='')
			cmd <- paste(cmd, if (par.name == 'lambda' && package == 'rworldmap') 
						', catMethod="pretty",  numCats=20' else '', ')', sep='')
		}
		create.script.widget(cmd, h$action$mw, package="bayesTFR")
	} else {
		pred <- do.call('get.tfr.prediction', param.pred)
		if (par.name == 'TFR' && package == 'rworldmap') {
			param.map <-  get.tfr.map.parameters(pred, same.scale=same.scale, quantile=quantile)
		} else {
			param.map <- list(pred=pred, quantile=quantile)
			if (par.name != 'TFR')
				param.map[['par.name']]<- par.name
				if(par.name=='lambda' && package == 'rworldmap') 
					param.map <- c(param.map, list(catMethod='pretty',  numCats=20))
		}
		if(package == 'rworldmap') param.map[['device']] <- 'dev.new'
		if (package == 'googleVis') param.map[['pi']] <- bounds
		g <- create.graphics.map.window(parent=h$action$mw, pred=pred, params=param.map, percentile=percentile, 
										is.gvis= package == 'googleVis', title="World Map")
	}
}

	
create.graphics.map.window <- function(parent, pred, params, percentile,  is.gvis=FALSE, title='', type='tfr', 
											main.part=NULL, dpi=80) {
	meta <- pred$mcmc.set$meta
	est.periods <- bayesTFR:::get.tfr.periods(meta)
	proj.periods <- bayesTFR:::get.prediction.periods(meta, pred$nr.projections+1)
	
	newMap <- function(h, ...) {
		if (!is.null(h$action$dev)) dev.set(h$action$dev)
		if(!is.null(h$action$map.pars$device)) h$action$map.pars$device <- "dev.cur"
		do.show.map(as.numeric(svalue(proj.year)), h$action$map.pars)
	}
	do.show.map <- function(projection.year, map.pars, update.control.win=TRUE) {
		#is.median <- percentile == 'median'
		ind.proj <- bayesTFR:::get.predORest.year.index(pred, projection.year)
		#projection.index <- ind.proj['index']
		is.projection <- ind.proj['is.projection']
		if(update.control.win)
			svalue(year.label) <- if(is.projection) 'Projection year:' else 'Estimation year:'
		#measure <- if(is.null(params$par.name)) toupper(type) else params$par.name
		#main <- paste(if(is.projection) proj.periods[projection.index] else est.periods[projection.index], 
		#			if(is.null(main.part)) measure else main.part, ":",
		#			if(is.median) percentile else paste(substr(percentile, 1, 5), ' bound of ',
		#						substr(percentile, 7,8), '% interval', sep=''))
		do.call(paste(type, '.map', if(is.gvis) '.gvis' else '', sep=''), 
				c(map.pars, list(projection.year=projection.year #, main=main
					)))
	}
	close.map <- function(h, ...) dev.off(h$action$dev)
	
	if(is.gvis && !is.null(params[['par.name']])) {
		do.show.map(meta$present.year, params, update.control.win=FALSE)
		return(NULL)
	}
	lest.periods <- length(est.periods)
	periods <- c(est.periods[-lest.periods], # remove the present period, otherwise doubled 
				 proj.periods)
	est.years <- bayesTFR:::get.estimation.years(meta)
	years <- c(est.years[-lest.periods], bayesTFR:::get.all.prediction.years(pred))
	e <- new.env()
	win <- gwindow(paste(title, 'Control Panel:', percentile), height=70, parent=parent, horizontal=FALSE)
	g <- ggroup(container=win, horizontal=FALSE, expand=TRUE)
	g1 <- ggroup(container=g, horizontal=TRUE)
	year.label <- glabel("Projection year:", container=g1)
	proj.year <- gspinbutton(from= min(years), to=max(years), by=5, value=years[lest.periods], container=g1)
	if(!is.null(params$par.name)) enabled(proj.year) <- FALSE
	do.show.map(meta$present.year, params)
	if (!is.gvis) {
		addSpring(g1)
		glabel("Output type:", container=g1)
		e$type <- gdroplist(c("pdf", "postscript", "png", "jpeg", "tiff", "bmp"), container=g1)
		height <- list()
		height[['png']] <- height[['jpeg']] <- height[['tiff']] <- height[['bmp']] <- 500
		height[['pdf']] <- height[['postscript']] <- 7
		e$height <- height
		e$width <- 'default'
		gb <- gbutton('Save', container=g1)
		addHandlerClicked(gb, handler=saveGraph, action=list(mw=win, env=e, dpi=dpi, dev=dev.cur()))
		addHandlerChanged(proj.year, handler=newMap, action=list(dev=dev.cur(), map.pars=params))
		addHandlerDestroy(win, handler=close.map, action=list(dev=dev.cur()))
	} else {
		addHandlerChanged(proj.year, handler=newMap, action=list(map.pars=params))
	}
	return(g)
}
	

showDLcurve <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(sim.dir='Simulation directory'), env=e)) return()
	country.pars <- get.country.code.from.widget(e$dlc.country$country.w, e$dlc.country)
	if(is.null(country.pars)) return(NULL)
	param.env <-list(sim.dir=svalue(e$sim.dir), pi=svalue(e$dlc.ci), country=country.pars$code, 
					 nr.curves=svalue(e$dlc.nr.curves), burnin=svalue(e$dlc.bi),
					 tfr.max=svalue(e$dlc.tfr.max), predictive.distr=svalue(e$predictive.distr),
					 output.dir=country.pars$output.dir, output.type=country.pars$output.type,
					 verbose=TRUE
						)
	param.names1 <- list(text='sim.dir')
	param.names.graph <- list(numvector='pi', numeric=c('country', 'nr.curves', 'burnin', 'tfr.max'), logical=c('predictive.distr'))
	param.mcmc <- get.parameters(param.names1, env=param.env, quote=h$action$script, retrieve.from.widgets=FALSE)
	if(h$action$script) {
		cmd <- paste('pred <- get.tfr.prediction(', paste(paste(names(param.mcmc), param.mcmc, sep='='), collapse=', '), 
						')\n', sep='')
	} else {
		pred <- do.call('get.tfr.prediction', param.mcmc)
		cmd <- ''
	}
	pars.value <- svalue(e$dlc.graph.pars)
	if(!is.null(country.pars$code)) { # one country
		param.plot1c <- get.parameters(param.names.graph, env=param.env, quote=h$action$script, retrieve.from.widgets=FALSE)
		cmd <- paste(cmd, 'DLcurve.plot(mcmc.list=pred, ',
						paste(paste(names(param.plot1c), param.plot1c, sep='='), collapse=', '), ', ',
						pars.value, ')', sep='')
		if (h$action$script) {
			script.text <- gwindow('bayesTFR commands', parent=h$action$mw)
			gtext(cmd, container=script.text)
		} else {
			create.graphics.window(parent=h$action$mw, title=paste("Double Logistic Curves for", country.pars$name))
			eval(parse(text=cmd))
		}
	} else { # all countries
		param.names.graph[['text']] <- c('output.dir', 'output.type')
		param.names.graph$logical <- c(param.names.graph$logical, 'verbose')
		param.plot.allc <- get.parameters(param.names.graph, env=param.env, quote=h$action$script, retrieve.from.widgets=FALSE)
		cmd <- paste(cmd, 'DLcurve.plot.all(mcmc.list=pred, ', 
						paste(paste(names(param.plot.allc), param.plot.allc, sep='='), collapse=', '), ', ',
					pars.value, ')', sep='')
		if (h$action$script) {
			script.text <- gwindow('bayesTFR commands', parent=h$action$mw)
			gtext(cmd, container=script.text)
		} else {
			eval(parse(text=cmd))
		}
	}
}

showParTraces <- function(h, ...) {
	e <- h$action$env
	cs <- svalue(e$traces.cs.chb, index=TRUE)
	dir <- svalue(e$sim.dir)
	burnin <- svalue(e$traces.burnin)
	thin <- svalue(e$traces.thin)
	print.summary <- h$action$print.summary
	if (cs==2) {
		country.pars <- get.country.code.from.widget(e$traces.country$country.w, e$traces.country)
		if(is.null(country.pars)) return(NULL)
	}	
	all.pars <- svalue(e$traces.pars.chb)
	nr.points <- svalue(e$traces.nr.points)
	if(print.summary) {
		warn <- getOption('warn')
		options(warn=-1) # disable warning messages
		mcmc.set <- get.tfr.mcmc(dir)
		options(warn=warn)
		con <- textConnection("mc.summary", "w", local=TRUE)
		mc.exist <- TRUE
		sink(con)
		if (is.null(mcmc.set)) {
			cat('No simulation available in this directory.')
			mc.exist <- FALSE
		}
	} else create.graphics.window(parent=h$action$mw, title="Parameter traces", dpi=150)
	if (cs==2) { # country-specific parameters
		if (!all.pars) {
			pars <- svalue(e$traces.par.cs.dl)
			if(print.summary) {if (mc.exist) print(summary(mcmc.set, country=country.pars$code, par.names.cs=pars, par.names=NULL, 
											burnin=burnin, thin=thin))
			} else 
			tfr.partraces.cs.plot(sim.dir=dir, country=country.pars$code, par.names=pars, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		} else {
			if(print.summary){if (mc.exist) print(summary(mcmc.set, country=country.pars$code, par.names=NULL, 
											burnin=burnin, thin=thin))
			} else 
			tfr.partraces.cs.plot(sim.dir=dir, country=country.pars$code, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		}
	} else { # World-parameters
		if (!all.pars) { # selected pars
			pars <- svalue(e$traces.par.dl)
			if(print.summary) {if (mc.exist) print(summary(mcmc.set, par.names.cs=NULL, par.names=pars, 
											burnin=burnin, thin=thin))
			} else 
			tfr.partraces.plot(sim.dir=dir, par.names=pars, nr.points=nr.points, 
											burnin=burnin, thin=thin)
		} else { # all pars
			if(print.summary) {if (mc.exist) print(summary(mcmc.set, par.names.cs=NULL, 
											burnin=burnin, thin=thin))
			} else 
			tfr.partraces.plot(sim.dir=dir, nr.points=nr.points)
		}
	}
	if(print.summary) {
		sink()
		close(con)
		sum.win <- gwindow('MCMC summary', parent=h$action$mw, width=500, height=400)
		set.widget.bgcolor(sum.win, "white")
		gtext(mc.summary, container=sum.win)
	}
}

computeConvergenceDiag <- function(h, ...) {
	e <- h$action$env
	type <- h$action$type
	if(!has.required.arguments(list(sim.dir='Simulation directory', burnin='Burnin'), env=e)) return()
	param.names <- list(numeric=c('burnin', 'thin', 'country.sampling.prop'),
						text=c('sim.dir'),
						logical=c('express', 'verbose', 'keep.thin.mcmc'))
	params <- get.parameters(param.names, e, quote=h$action$script)
	if(params$express || params$country.sampling.prop >= 1) params$country.sampling.prop <- NULL
	if (h$action$script) {
		cmd <- paste(type, '.diagnose(', paste(paste(names(params), params, sep='='), collapse=', '), ', ',
					paste(paste(names(e$params), e$params, sep='='), collapse=', '), ')',sep='')
		create.script.widget(cmd, h$action$mw, package=h$action$package)
	} else {
		run <- FALSE
		mcmc.set <- do.call(paste('get.', type, '.mcmc', sep=''), list(sim.dir=params$sim.dir))
		iter <- get.total.iterations(mcmc.set$mcmc.list, burnin=params$burnin)
		if (iter < 0) gmessage('Number of iterations is smaller than burnin. Change the value of burnin.',
							container=h$action$mw)
		else {
			if (iter > 10000) {
				gconfirm('Computing convergence diagnostics with these settings can take a very long time. Do you want to continue?',
					icon='question', parent=h$action$mw,
					handler=function(h1, ...) run <<- TRUE)
			} else run <- TRUE
			if(run) do.call(paste(type, '.diagnose', sep=''), c(params))
		}
	}
}

showConvergenceDiag <- function(h, ...) {
	e <- h$action$env
	type <- h$action$type
	dir <- svalue(e$sim.dir)
	diag.all <- do.call(paste('get.', type, '.convergence.all', sep=''), list(dir))
	ldiag <- length(diag.all)
	if(ldiag <=0) {
		gmessage(paste('There is no available convergence diagnostics in', dir), container=h$action$mw)
		return()
	}
	path = system.file("images",package="bayesDem")
	win <- gwindow('Available Convergence Diagnostics', parent=h$action$mw)
	g <- ggroup(horizontal=FALSE, container=win)
	verb <- 'are'
	noun.postfix <- 's'
	if(ldiag == 1) {
		verb <- 'is'
		noun.postfix <- ''
	}
	glabel(paste('There ', verb, ' ', ldiag, ' set', noun.postfix, ' of diagnostics.', sep=''),
			container=g)
	glabel('Click on the traffic lights to get a report.', container=g)
	addSpace(g, 10)
	glo <- glayout(container=g)
	glo[4,1] <- glabel('Needs at least', container=glo)
	l <- 1
	for(i in 1:ldiag) {
		diag <- diag.all[[i]]
		light <- names(diag$status)[diag$status]
		if(length(light) > 1) light <- 'green-yellow'
		image.name <- file.path(path, 'traffic_light', paste(light, 'png', sep='.'))
		glo[l,i+1] <- glabel(paste('Burnin =', diag$burnin), container=glo)
		glo[l+1,i+1] <- glabel(paste('Thin =', diag$thin), container=glo)
		glo[l+2,i+1] <- gimage(image.name, container=glo, handler=showDiagInfo, 
								action=list(mw=win, env=e, diag=diag))
		glo[l+3,i+1] <- glabel(diag$iter.needed, container=glo)
	}
	glo[4,ldiag+2] <- glabel('additional iterations', container=glo)
	addSpace(g,20)
}

showDiagInfo <- function(h, ...) {
	diag <- h$action$diag
	con <- textConnection("conv.diag", "w", local=TRUE)
	sink(con)
	summary(diag, expand=TRUE)
	sink()
	close(con)
	win <- gwindow(paste('Convergence Diagnostics for burnin=', diag$burnin, sep=''), 
						parent=h$action$mw, width=500, height=400)
	gtext(conv.diag, container=win)
}