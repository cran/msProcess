## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSDenoise.q#11 $
## $DateTime: 2008/05/13 13:33:48 $

menuMSDenoise = function(x, 							#1 mother function
						 FUN = "wavelet", 				#2 mother function
						 attach.noise = T, 				#3 mother function
						 event = "Denoising",			#4 mother function
						 twiceit = T,   				#5 smooth
						 assign.attributes = T, 		#6 wavelet & waveletThresh
						 n.level = as.integer(floor(logb(length(x), 2))),  #7 wavelet & waveletThresh
						 shrink.fun = "hard",  			#8 wavelet & waveletThresh
						 thresh.fun = "universal",  	#9 wavelet & waveletThresh
						 thresh.scale = 1,  			#10 wavelet & waveletThresh
						 noise.variance = NULL,			#11 wavelet & waveletThresh
						 wavelet = "s8", 				#12 wavelet & waveletThresh
						 xform = "modwt",   			#13 wavelet & waveletThresh
						 reflect = T, 					#14 wavelet & waveletThresh
						 waveletMRD = "s8", 			#15 MRD
						 xformMRD = "modwt",   			#16 MRD
						 reflectMRD = T, 				#17 MRD
						 levels = 1, 					#18 MRD
						 keep.details = T, 				#19 MRD
						 keep.smooth = T, 				#20 MRD
						 saveAs = paste(deparse(substitute(x)), ".denoise", sep = ""),	#21
						 printObj = T, 					#22 display tab
						 printHistory = T, 				#23 display tab
						 plotResult = T, 				#24 display tab
						 plot.xaxis.variable = "time",	#25 display tab
						 plot.spectra.subset = 1,		#26 display tab
						 plot.spectra.offset = NULL, 	#27 display tab
						 imageResult = T,				#28 display tab
						 image.xaxis.variable = "time",	#29 display tab					 
						 image.spectra.subset = NULL	#30 display tab
						 ){
						 	
	out = switch(FUN,
			"smooth" = msDenoise(x = x, FUN = "smooth", twiceit = twiceit, 
								 attach.noise = attach.noise, process = "msDenoiseSmooth"),
								 
			"mrd" = {
				if(is.all.white(levels)){
					guiDisplayMessageBox("Must enter a set of positive integers for the levels.",
      									 button = c("Ok"),
										 icon = c("error"))
				}
				if(length(grep(",", levels))){
					if(length(grep("c", levels))) levels = eval(parse(text = levels))
					else levels = eval(parse(text = paste("c(", levels, ")")))
					
				} else  { #if(length(grep(" ", levels)))
						ll = unlist(unpaste(levels, sep = " "))
						ll = ll[ll != ""]
						levels = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
				}
										 						 
				msDenoise(x = x, FUN = "mrd", attach.noise = attach.noise,
							  event = event, wavelet = waveletMRD, xform = xformMRD, 
							  reflect = reflectMRD, levels = levels, keep.details = keep.details, 
							  keep.smooth = keep.smooth, process="msDenoiseMRD")
			}, 				 
			"wavelet" = {
				thresh.scale = as.numeric(thresh.scale)
				n.level = as.numeric(n.level)
				if(!is.null(noise.variance)) noise.variance = as.numeric(noise.variance)
				msDenoise(x = x, FUN = "wavelet", attach.noise = attach.noise,
								  event = event, assign.attributes = assign.attributes,
								  n.level = n.level, shrink.fun = shrink.fun, thresh.fun = thresh.fun,
								  thresh.scale = thresh.scale, noise.variance = noise.variance, 
								  wavelet = wavelet, xform = xform, reflect = reflect,
								  process="msDenoiseWavelet")
			})
								  
	assign(saveAs, out, where = 1)
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	# plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msDenoise",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	## image options
	
	if(imageResult){
		imageFromGUI(out,
				 	 what = "noise",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()
}
