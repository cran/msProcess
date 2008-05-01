## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSDetrend.q#1 $
## $DateTime: 2008/04/30 16:28:52 $

backMSDetrend = function(data){
	
	assign("propData", data, where = 1)
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSDetrendDataSet", paste(objects(classes = "msSet"), collapse = ","))	
	}

	## actions based on selecting the data set
	if(activeprop == "MSDetrendDataSet"){
		if(exists(cbGetCurrValue(data, "MSDetrendDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSDetrendSaveAs", 
							  paste(cbGetCurrValue(data, "MSDetrendDataSet"), ".noise", sep = ""))
			data = cbSetOptionList(data, "MSDetrendDataType", paste(names(get(cbGetCurrValue(data, "MSDetrendDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSDetrendDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
	}

	if(activeprop == "MSDetrendFUN" && !exists(cbGetCurrValue(data, "MSDetrendFUN"))){
      		guiDisplayMessageBox(paste( cbGetCurrValue(data, "MSDetrendFUN"), 
      							 		"does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}						
		
	if(activeprop == "MSDetrendPre" && !exists(cbGetCurrValue(data, "MSDetrendPre"))){
      		guiDisplayMessageBox(paste( cbGetCurrValue(data, "MSDetrendPre"), 
      							 		"does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}						
  data	
	
}
