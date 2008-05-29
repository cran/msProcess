## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSNormalize.q#1 $
## $DateTime: 2008/05/01 15:25:42 $

backMSNormalize = function(data){
	
	assign("propData", data, where = 1)
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSNormalizeDataSet", paste(objects(classes = "msSet"), collapse = ","))	
	}

	## actions based on selecting the data set
	if(activeprop == "MSNormalizeDataSet"){
		if(exists(cbGetCurrValue(data, "MSNormalizeDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSNormalizeSaveAs", 
							  paste(cbGetCurrValue(data, "MSNormalizeDataSet"), ".norm", sep = ""))
			data = cbSetOptionList(data, "MSNormalizeDataType", paste(names(get(cbGetCurrValue(data, "MSNormalizeDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSNormalizeDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
	}

  data	
}
