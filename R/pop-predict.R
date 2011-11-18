popNewPred.group <- function(g, main.win, parent) {
	e <- new.env()
	defaults <- formals(pop.predict) # default argument values
	e$output.dir <- e$sim.dir <- parent$sim.dir
	
	pred.g <- gframe("<span color='blue'>Prediction</span>", markup=TRUE, horizontal=FALSE, cont=g)
	pred.g1 <- ggroup(horizontal=TRUE, cont=pred.g)
	glabel("End year:", cont=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=pred.g1)
	e$end.year <- gedit(defaults$end.year, width=4, cont=pred.g1)
	addSpace(pred.g1, 5)
	glabel("Start year:", cont=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=pred.g1)
	e$start.year <- gedit(defaults$start.year, width=4, cont=pred.g1)
	addSpace(pred.g1, 5)
	glabel("Present year:", cont=pred.g1)
	glabel("<span color='red'>*</span>", markup=TRUE, cont=pred.g1)
	e$present.year <- gedit(defaults$present.year, width=4, cont=pred.g1)
	glabel("     WPP year:", cont=pred.g1)
	glabel(parent$wpp.year, cont=pred.g1)
	
	pred.g2 <- ggroup(horizontal=TRUE, cont=pred.g)
	glabel("Nr. of trajectories:", cont=pred.g2)
	e$nr.traj <- gedit(defaults$nr.traj, width=5, cont=pred.g2)
	addSpace(pred.g2, 5)
	e$replace.output <- gcheckbox("Overwrite existing prediction", 
									checked=defaults$replace.output, cont=pred.g2)
	addSpace(pred.g2, 5)
	e$verbose <- gcheckbox("Verbose", checked=defaults$verbose, cont=pred.g2)
	
	countries.g <- gframe("<span color='blue'>Countries selection</span>", markup=TRUE, 
							horizontal=FALSE, cont=g)
	countries.g1 <- ggroup(horizontal=TRUE, cont=countries.g)
	e$all.countries <- gcheckbox("All countries", checked=TRUE, cont=countries.g1,
									handler=function(h,...){
										enabled(countries.gb) <- !svalue(h$obj)
										enabled(e$all.remaining.countries) <- !svalue(h$obj)
										})
	addSpace(countries.g1, 20)
	countries.gb <- gbutton("  Select countries  ", cont=countries.g1,
				handler=selectCountryMenuPop,
				action=list(mw=main.win, env=e, multiple=TRUE, wpp.year=parent$wpp.year))
	addSpace(countries.g1, 10)
	e$all.remaining.countries <- gcheckbox("All countries without prediction", checked=FALSE, cont=countries.g1,
									handler=function(h,...){
										enabled(countries.gb) <- !svalue(h$obj)
										enabled(e$all.countries) <- !svalue(h$obj)
										})
	enabled(countries.gb) <- !svalue(e$all.countries)
	enabled(e$all.remaining.countries) <- !svalue(e$all.countries)
	
#	e$inputs <- list()				
#	input.g <- gframe("<span color='blue'>Projection inputs</span>", markup=TRUE, 
#							horizontal=FALSE, cont=g)
#	input.g1 <- ggroup(horizontal=TRUE, cont=input.g)
#	input.g1.fert <- gframe("<span color='#0B6138'>Fertility</span>", markup=TRUE, 
#							horizontal=FALSE, cont=input.g1)
#	input.g.fert1 <- ggroup(horizontal=TRUE, cont=input.g1.fert)
#	glabel("TFR:   ", cont=input.g.fert1)
#	#glabel("<span color='red'>*  </span>", markup=TRUE, cont=input.g.fert1)
#	e$inputs$tfr.sim.dir <- gfilebrowse(eval(defaults$inputs$tfr.sim.dir), type='open', 
#					  width=19, quote=FALSE, cont=input.g.fert1)
#	input.g.fert2 <- ggroup(horizontal=TRUE, cont=input.g1.fert)
#	glabel("PASFR:", cont=input.g.fert2)
#	e$inputs$pasfr <- gfilebrowse(eval(defaults$inputs$pasfr), type='open', 
#					  width=19, quote=FALSE, cont=input.g.fert2)
#	input.g.e0 <- gframe("<span color='#0B6138'>Life Expectancy</span>", markup=TRUE, 
#							horizontal=FALSE, cont=input.g1)
#	input.g.e01 <- ggroup(horizontal=TRUE, cont=input.g.e0)
#	glabel("e0 male:   ", cont=input.g.e01)
#	#glabel("<span color='red'>*   </span>", markup=TRUE, cont=input.g.e01)
#	e$inputs$e0M.sim.dir <- gfilebrowse(eval(defaults$inputs$e0M.sim.dir), type='selectdir', 
#					  width=19, quote=FALSE, cont=input.g.e01)
#	input.g.e02 <- ggroup(horizontal=TRUE, cont=input.g.e0)
#	glabel("e0 female:", cont=input.g.e02)
#	#glabel("<span color='red'>*</span>", markup=TRUE, cont=input.g.e02)
#	e$inputs$e0F.sim.dir <- gfilebrowse(eval(defaults$inputs$e0F.sim.dir), type='selectdir', 
#					  width=19, quote=FALSE, cont=input.g.e02)
	
	addSpace(g, 20)
	gbutton("Projection inputs", markup=TRUE, cont=g,
				handler=OptInputFilesPopPred,
				action=list(mw=main.win, env=e, defaults=defaults))

	addSpring(g)
	button.g <- ggroup(horizontal=TRUE, cont=g)
	create.help.button(topic='pop.predict', package='bayesPop', 
				parent.group=button.g,
						parent.window=main.win)
	addSpring(button.g)
	gbutton(' Generate Script ', cont=button.g, handler=run.pop.prediction,
				action=list(mw=main.win, env=e, script=TRUE, wpp.year=parent$wpp.year))
	gbutton(action=gaction(label=' Make Prediction ', icon='evaluate', handler=run.pop.prediction, 
				action=list(mw=main.win, env=e, script=FALSE, wpp.year=parent$wpp.year)), 
				cont=button.g)
	return(e)					
}

run.pop.prediction <- function(h, ...) {
	e <- h$action$env
	if(!has.required.arguments(list(output.dir='Simulation directory', end.year='End year', 
									start.year='Start year', present.year='Present year'), env=e)) return()
	#if(!has.required.arguments(list(tfr.sim.dir='TFR', e0M.sim.dir='e0 male',
	#								e0F.sim.dir='e0 female'), env=e$inputs)) return()
	param.names <- list(numeric=c('end.year', 'start.year', 'present.year', 'nr.traj'), 
						text=c('output.dir'),
						logical=c('verbose', 'replace.output')
						)
	param.input.names.opt <- list(
		text=c('tfr.sim.dir', 'tfr.file', 'e0M.sim.dir', 'e0M.file', 'e0F.sim.dir', 'e0F.file',
				'popM', 'popF', 'mxM', 'mxF', 'srb', 'migM', 'migF', 'mig.type'))
	params <- get.parameters(param.names, e, h$action$script)
	params.inp <- list()
	if(!is.null(e$opt) && e$inputs.modified) {
		opt.names <- list(text=c())
		for (par in param.input.names.opt$text)	
			if (is.element(par, names(e$inputs)) && !is.null(e$inputs[[par]])) 
				opt.names$text <- c(opt.names$text, par)
		params.inp <- c(params.inp, get.parameters(opt.names, e$inputs, h$action$script, 
							retrieve.from.widgets=FALSE))
	}
	params$wpp.year <- h$action$wpp.year
	params$countries <- NULL
	if (!svalue(e$all.countries)) {
		if (svalue(e$all.remaining.countries)) params$countries <- NA
		else params$countries <- e$selected.countries
	}
	if (h$action$script) {
		script.text <- gwindow('bayesPop commands', parent=h$action$mw)
		cmd <- paste('pop.predict(', paste(paste(names(params), params, sep='='), collapse=', '), sep=' ')
		if (length(params.inp) > 0)
			cmd <- paste(cmd, ', inputs = list(', paste(paste(names(params.inp), params.inp, sep='='), collapse=', '),')', sep=' ')
		cmd <- paste(cmd, ')', sep='')
		gtext(cmd, cont=script.text)
	} else {
		if(!params[['replace.output']] && has.pop.prediction(sim.dir=params[['output.dir']])) {
				gmessage(paste('Prediction for', params[['output.dir']], 
								'already exists.\nCheck "Overwrite existing prediction" to delete it.'))
				return()
		}
		do.call('pop.predict', c(params, if (length(params.inp) > 0) list(inputs=params.inp) else NULL))
	}

}

OptInputFilesPopPred <- function(h, ...) {
	input.names <- c('tfr.sim.dir', 'tfr.file', 'e0M.sim.dir', 'e0M.file', 'e0F.sim.dir', 'e0F.file',
				'popM', 'popF', 'mxM', 'mxF', 'srb', 'migM', 'migF', 'mig.type')
	defaults <- h$action$defaults
	
	setOptionalInputsPop <- function(h1, ...) {
		for(par in input.names) {
			value <- svalue(h$action$env$opt[[par]])
			if (nchar(value) > 0 || ((nchar(value)==0) && is.element(par, names(h$action$env$inputs)) && nchar(h$action$env$inputs[[par]]) > 0))
				h$action$env$inputs[[par]] <- value
		}
		h$action$env$inputs.modified <- TRUE
		visible(h$action$env$inputs.win) <- FALSE
	}
	if (!is.null(h$action$env$inputs.win) && !h$action$env$opt$window.destroyed) { #Window exists
		if(h$action$env$inputs.modified) { # OK button previously clicked
			for(par in input.names) 
				if(is.element(par, names(h$action$env$inputs))) 
					svalue(h$action$env$opt[[par]]) <- h$action$env$inputs[[par]]
		} else # OK button not clicked yet, values are set to defaults
			for(par in input.names) svalue(h$action$env$opt[[par]]) <- defaults$inputs[[par]]
		visible(h$action$env$inputs.win) <- TRUE
	} else { # create the inputs window
		h$action$env$inputs.win <- win <- gwindow('Select optional input files and directories', 
							parent=h$action$mw)
		h$action$env$inputs.modified <- FALSE
		e <- new.env() # h$action$env
		g <- ggroup(horizontal=FALSE, cont=win)
		g.tfr <- gframe("<span color='#0B6138'>Total Fertility Rate (select one)</span>", markup=TRUE, 
							horizontal=TRUE, cont=g)
		glabel("bayesTFR sim folder:", cont=g.tfr)
		e$tfr.sim.dir <- gfilebrowse(eval(defaults$inputs$tfr.sim.dir), type='selectdir', 
					  width=20, quote=FALSE, cont=g.tfr)
		glabel("CSV file:", cont=g.tfr)
		e$tfr.file <- gfilebrowse(eval(defaults$inputs$tfr.sim.dir), type='open', 
					  width=20, quote=FALSE, cont=g.tfr)
					  
		g.e0m <- gframe("<span color='#0B6138'>Male Life Expectancy (select one)</span>", markup=TRUE, 
							horizontal=TRUE, cont=g)
		glabel("bayesLife sim folder:", cont=g.e0m)
		e$e0M.sim.dir <- gfilebrowse(eval(defaults$inputs$e0M.sim.dir), type='selectdir', 
					  width=20, quote=FALSE, cont=g.e0m)
		glabel("CSV file:", cont=g.e0m)
		e$e0M.file <- gfilebrowse(eval(defaults$inputs$e0M.sim.dir), type='open', 
					  width=20, quote=FALSE, cont=g.e0m)
					  
		g.e0f <- gframe("<span color='#0B6138'>Female Life Expectancy (select one)</span>", markup=TRUE, 
							horizontal=TRUE, cont=g)
		glabel("bayesLife sim folder:", cont=g.e0f)
		e$e0F.sim.dir <- gfilebrowse(eval(defaults$inputs$e0F.sim.dir), type='selectdir', 
					  width=20, quote=FALSE, cont=g.e0f)
		glabel("CSV file:", cont=g.e0f)
		e$e0F.file <- gfilebrowse(eval(defaults$inputs$e0F.sim.dir), type='open', 
					  width=20, quote=FALSE, cont=g.e0f)
		
		g.other <- gframe("<span color='#0B6138'>Other Optional Files</span>", markup=TRUE, 
							horizontal=FALSE, cont=g)
		glo <- glayout(cont=g.other)
		glo[1,1] <- glabel("Initial Male Population:", cont=glo)
		glo[1,2] <- e$popM <- gfilebrowse(eval(defaults$inputs$popM), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[2,1] <- glabel("Initial Female Population:", cont=glo)
		glo[2,2] <- e$popF <- gfilebrowse(eval(defaults$inputs$popF), type='open', 
					  width=50, quote=FALSE, cont=glo)
					  
		glo[3,1] <- glabel("Mortality Rate Male:", cont=glo)
		glo[3,2] <- e$mxM <- gfilebrowse(eval(defaults$inputs$mxM), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[4,1] <- glabel("Mortality Rate Female:", cont=glo)
		glo[4,2] <- e$mxF <- gfilebrowse(eval(defaults$inputs$mxF), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[5,1] <- glabel("Sex Ratio at Birth:", cont=glo)
		glo[5,2] <- e$srb <- gfilebrowse(eval(defaults$inputs$srb), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[6,1] <- glabel("% Age-specific Fertility Ratio:", cont=glo)
		glo[6,2] <- e$pasfr <- gfilebrowse(eval(defaults$inputs$pasfr), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[7,1] <- glabel("Migration Male:", cont=glo)
		glo[7,2] <- e$migM <- gfilebrowse(eval(defaults$inputs$migM), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[8,1] <- glabel("Migration Female:", cont=glo)
		glo[8,2] <- e$migF <- gfilebrowse(eval(defaults$inputs$migF), type='open', 
					  width=50, quote=FALSE, cont=glo)
		glo[9,1] <- glabel("Migration Type:    ", cont=glo)
		glo[9,2] <- e$mig.type <- gfilebrowse(eval(defaults$inputs$mig.type), type='open', 
					  width=50, quote=FALSE, cont=glo)

		b.group <- ggroup(horizontal=TRUE, cont=g)
		gbutton('Cancel', cont=b.group, handler=function(h1, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		e$okbutton <- gbutton('OK', cont=b.group)
		e$window.destroyed <- FALSE
		e$inputs.modified <- FALSE
		h$action$env$opt <- e
		addHandlerDestroy(win, handler=function(h1, ...) h$action$env$opt$window.destroyed <- TRUE)
	}
	if(!is.null(h$action$env$opt.okhandler)) 
		removehandler(h$action$env$opt$okbutton, h$action$env$opt.okhandler)
	h$action$env$opt.okhandler <- addhandlerclicked(h$action$env$opt$okbutton, 
											handler=setOptionalInputsPop)
}



selectCountryMenuPop <- function(h, ...) {
	country.selected <- function(h1, ...) {
		h$action$env$selected.countries <- svalue(h$action$env$country.gt)
		visible(h$action$env$country.sel.win) <- FALSE
	}
	if (!is.null(h$action$env$country.sel.win)) 
			visible(h$action$env$country.sel.win) <- TRUE
	else {
		SRB <- bayesPop:::read.bayesPop.file(paste('SexRatioAtBirth', h$action$wpp.year, '.txt', sep=''))
		country.table <- SRB[,c("country_code", "country")]
		h$action$env$country.table <- country.table
		h$action$env$country.sel.win <- win <- gwindow('Select countries', 
							parent=h$action$mw, height=450,
							handler=function(h, ...) {
								h$action$env$country.sel.win<-NULL;
								h$action$env$country.ok.handler <- NULL
							},
							action=list(env=h$action$env))
		t.group <- ggroup(horizontal=FALSE, cont=win)
		h$action$env$country.gt <- gtable(h$action$env$country.table, cont=t.group, 
					expand=TRUE, multiple=h$action$multiple, handler=country.selected)
		b.group <- ggroup(horizontal=TRUE, cont=t.group)
		gbutton('Cancel', cont=b.group, handler=function(h, ...) 
					visible(win) <- FALSE)
		addSpring(b.group)
		h$action$env$country.okbutton <- gbutton('OK', cont=b.group)
	}
	if(!is.null(h$action$env$country.ok.handler)) 
		removehandler(h$action$env$country.okbutton, h$action$env$country.ok.handler)
	h$action$env$country.ok.handler <- addhandlerclicked(
						h$action$env$country.okbutton, handler=country.selected)

}
