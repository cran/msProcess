## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSNoise.q#3 $
## $DateTime: 2008/04/30 10:44:24 $

menuMSNoise = function(x, 									#1 
						 type = "noise", 					#2 
						 FUN = "spline", 					#3 
						 pre = "abs",						#4
						 MARGIN = 2, 						#5
						 event = "Local Noise Estimation",	#6
						 detach.noise = T, 					#7
						 printObj = T, 						#8 
						 printHistory = T,					#9 
						 saveAs = paste(deparse(substitute(x)), ".noise", sep = "")	#10	 
						 ){
						 	

	out = msNoise(x = x, 
				  FUN = FUN, 
				  MARGIN = MARGIN, 
				  type = type, 
				  pre = get(pre), 
				  detach.noise = detach.noise, 
				  event = event)
				  
	assign(saveAs, out, where = 1)
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	invisible()	
}
