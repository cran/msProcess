## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSPrepare.q#6 $
## $DateTime: 2008/04/30 10:25:31 $

backMSPrepare = function(data){
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)
	# set startup properties	
	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSPrepDataSet", paste(objects(classes = "msList"), collapse = ","))
	}

	if(activeprop == "MSPrepDataSet"){
		if(exists(cbGetCurrValue(data, "MSPrepDataSet"))){
			data = cbSetCurrValue(data, 
							  	  "MSPrepSaveAs", 
							  	  paste(cbGetCurrValue(data, "MSPrepDataSet"), ".prep", sep = ""))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSPrepDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							 button = c("Ok"),
								 icon = c("error"))				
		}
	}
	msPrepTrans = cbGetCurrValue(data, "MSPrepTransform")
	if(activeprop == "MSPrepTransform" && 
	   !is.element(msPrepTrans, c("<None>", "cubert")) && 
	   !exists(cbGetCurrValue(data, "MSPrepTransform"))){
      guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSPrepTransform"), 
      							 "does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}
data
}
