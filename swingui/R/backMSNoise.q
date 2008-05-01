## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSNoise.q#4 $
## $DateTime: 2008/04/30 10:44:24 $

backMSNoise = function(data){
	
	assign("propData", data, where = 1)
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSNoiseDataSet", paste(objects(classes = "msSet"), collapse = ","))	
	}

	## actions based on selecting the data set
	if(activeprop == "MSNoiseDataSet"){
		if(exists(cbGetCurrValue(data, "MSNoiseDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSNoiseSaveAs", 
							  paste(cbGetCurrValue(data, "MSNoiseDataSet"), ".noise", sep = ""))
			data = cbSetOptionList(data, "MSNoiseDataType", paste(names(get(cbGetCurrValue(data, "MSNoiseDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSNoiseDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
	}

	if(activeprop == "MSNoiseFUN" && !exists(cbGetCurrValue(data, "MSNoiseFUN"))){
      		guiDisplayMessageBox(paste( cbGetCurrValue(data, "MSNoiseFUN"), 
      							 		"does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}						
		
	if(activeprop == "MSNoisePre" && !exists(cbGetCurrValue(data, "MSNoisePre"))){
      		guiDisplayMessageBox(paste( cbGetCurrValue(data, "MSNoisePre"), 
      							 		"does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}						
  data	
	
}
