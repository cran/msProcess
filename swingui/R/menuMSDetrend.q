## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSDetrend.q#1 $
## $DateTime: 2008/04/30 16:28:52 $

menuMSDetrend = function(x, 								#1 
						 type = "intensity", 				#2 
						 FUN = "loess", 					#3 
						 pre = NULL,						#4
						 MARGIN = 2, 						#5
						 event = "Baseline Correction",		#6
						 attach.base = T, 					#7
						 printObj = T, 						#8 
						 printHistory = T,					#9 
						 saveAs = paste(deparse(substitute(x)), ".noise", sep = "")	#10	 
						 ){
						 	

	if( pre == "<None>" || is.all.white(pre) ) pre = NULL
	else pre = get(pre)
	
	out = msDetrend(x = x, 
				  	FUN = FUN, 
				  	MARGIN = MARGIN, 
				  	type = type, 
				  	pre = pre, 
				  	attach.base = attach.base, 
				  	event = event)
				  
	assign(saveAs, out, where = 1)
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	invisible()	
}
