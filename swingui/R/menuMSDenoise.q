## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSDenoise.q#5 $
## $DateTime: 2008/04/28 14:42:12 $

menuMSDenoise = function(x, 							#1 mother function
						 type = "intensity", 			#2 mother function
						 FUN = "wavelet", 				#3 mother function
						 MARGIN = 2, 					#4 mother function
						 attach.noise = T, 				#5 mother function
						 event = "Denoising",			#6 mother function
						 printObj = T, 					#7 mother dialog
						 printHistory = T, 				#8 mother dialog
						 twiceit = T,   				#9 smooth
						 assign.attributes = T, 		#10 wavelet & waveletThresh
						 n.level = as.integer(floor(logb(length(x), 2))),  #11 wavelet & waveletThresh
						 shrink.fun = "hard",  			#12 wavelet & waveletThresh
						 thresh.fun = "universal",  	#13 wavelet & waveletThresh
						 thresh.scale = 1,  			#14 wavelet & waveletThresh
						 noise.variance = NULL,			#15 wavelet & waveletThresh
						 wavelet = "s8", 				#16 wavelet & waveletThresh
						 xform = "modwt",   			#17 wavelet & waveletThresh
						 reflect = T, 					#18 wavelet & waveletThresh
						 waveletMRD = "s8", 			#19 MRD
						 xformMRD = "modwt",   			#20 MRD
						 reflectMRD = T, 				#21 MRD
						 levels = 1, 					#22 MRD
						 keep.details = T, 				#23 MRD
						 keep.smooth = T, 				#24 MRD
						 saveAs = paste(deparse(substitute(x)), ".denoise", sep = "")	#25
						 ){
						 	
	out = switch(FUN,
			"smooth" = msDenoise(x = x, FUN = "smooth", type = type, twiceit = twiceit, MARGIN = MARGIN, 
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
										 						 
				msDenoise(x = x, FUN = "mrd", MARGIN = MARGIN, attach.noise = attach.noise,
							  event = event, type = type, wavelet = waveletMRD, xform = xformMRD, 
							  reflect = reflectMRD, levels = levels, keep.details = keep.details, 
							  keep.smooth = keep.smooth, process="msDenoiseMRD")
			}, 				 
			"wavelet" = {
				thresh.scale = as.numeric(thresh.scale)
				n.level = as.numeric(n.level)
				if(!is.null(noise.variance)) noise.variance = as.numeric(noise.variance)
				msDenoise(x = x, FUN = "wavelet", MARGIN = MARGIN, attach.noise = attach.noise,
								  event = event, type = type, assign.attributes = assign.attributes,
								  n.level = n.level, shrink.fun = shrink.fun, thresh.fun = thresh.fun,
								  thresh.scale = thresh.scale, noise.variance = noise.variance, 
								  wavelet = wavelet, xform = xform, reflect = reflect,
								  process="msDenoiseWavelet")
			})
								  
	assign(saveAs, out, where = 1)
	if(printObj) print(out)
	if(printHistory) print(summary(out))
	invisible()
}
	