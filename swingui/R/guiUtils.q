## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/guiUtils.q#2 $
## $DateTime: 2008/05/07 17:39:53 $

######################
####  plot.msSet  ####
######################

plotFromGUI = function(x,
					   process,
					   spectra.offset, 
					   spectra.subset,
					   xaxis.variable,
					   data.name = deparse(substitute(x))){
					   	
					   	
	if(!is.null(spectra.offset)){
		if(spectra.offset == "<Auto>" || is.all.white(spectra.offset)) spectra.offset = NULL
		else spectra.offset = as.numeric(spectra.offset)
	}
	if(!is.null(spectra.subset)){
		if(spectra.subset == "<All>" || is.all.white(spectra.subset)) spectra.subset = NULL	
		if(length(grep(",", spectra.subset))){
			if(length(grep("c", spectra.subset))) spectra.subset = eval(parse(text = spectra.subset))
			else spectra.subset = eval(parse(text = paste("c(", spectra.subset, ")")))
					
		} else  { #if(length(grep(" ", levels)))
				ll = unlist(unpaste(spectra.subset, sep = " "))
				ll = ll[ll != ""]
				spectra.subset = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
		}
	}
	
	## plot the spectra
	plot.msSet(	x, 
		 		process = process, 
		 		subset = spectra.subset, 
		 		offset = spectra.offset, 
		 		xaxis = xaxis.variable,
		 		main = paste(process, "for", data.name))		
		 			   	
	invisible()				   					   	
}

#######################
####  image.msSet  ####
#######################

imageFromGUI = function(x,
						what,
						spectra.subset,
						xaxis.variable,
						data.name = deparse(substitute(x))){
							
	if(!is.null(spectra.subset)){
		if(spectra.subset == "<All>" || is.all.white(spectra.subset)) spectra.subset = NULL	
		if(length(grep(",", spectra.subset))){
			if(length(grep("c", spectra.subset))) spectra.subset = eval(parse(text = spectra.subset))
			else spectra.subset = eval(parse(text = paste("c(", spectra.subset, ")")))
					
		} else  { #if(length(grep(" ", levels)))
			ll = unlist(unpaste(spectra.subset, sep = " "))
			ll = ll[ll != ""]
			spectra.subset = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
		}
	}
		image.msSet(x, 
			  what = what,
			  subset = spectra.subset,
			  xaxis = xaxis.variable,
			  main = paste(what, "for", data.name))							
						
	invisible()							
}
