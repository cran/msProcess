## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSPeak.q#1 $
## $DateTime: 2008/05/13 16:01:41 $

backMSPeak = function(data){
	
	assign("propData", data, where = 1)
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSPeakDataSet", paste(objects(classes = "msSet"), collapse = ","))	
	}

	## actions based on selecting the data set
	if(activeprop == "MSPeakDataSet"){
		if(exists(cbGetCurrValue(data, "MSPeakDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSPeakSaveAs", 
							  paste(cbGetCurrValue(data, "MSPeakDataSet"), ".peak", sep = ""))
			data = cbSetOptionList(data, "MSPeakDataType", paste(names(get(cbGetCurrValue(data, "MSPeakDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSPeakDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
	}

  data	
}
