## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSPeak.q#2 $
## $DateTime: 2008/05/15 12:43:23 $

menuMSPeak = function(x, 								#1 
					  FUN = "simple", 					#2 
					  event = "Peak Detection",			#3
					  us.mean = F,						#4
					  simple.SNRThreshold = 2,			#5, 
					  simple.span = 3,					#6,
					  search.SNRThreshold = 2,			#7, 
					  search.span = 3,					#8,
					  search.supsmuSpan = 3,			#9,					  
					  mrd.nLevel = floor(log2(numRows(x))),	#10
					  mrd.concvThreshold = 0,			#11
					  mrd.SNRThreshold = 2,				#12
					  saveAs = paste(deparse(substitute(x)), ".pkDetect", sep = ""),	#13						 											 
					  printObj = T, 					#14 display tab
					  printHistory = T,					#15 display tab
					  plotResult = T,					#16 display tab                       
					  plot.xaxis.variable = "time",		#17 display tab
					  plot.spectra.subset = 1,			#18 display tab
					  plot.spectra.offset = NULL, 		#19 display tab
					  imageResult = T,					#20 display tab
					  image.xaxis.variable = "time",	#21 display tab					 
					  image.spectra.subset = NULL		#22 display tab
					  ){
	
	out = msPeak(	x = x, 
				  	FUN = FUN, 
				  	event = event)
	
	## save		
	assign(saveAs, out, where = 1)
	
	## print if requested
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	## plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msPeak",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	## image options
	
	if(imageResult){
		imageFromGUI(out,
				 	 what = "peak.list",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()	
}
