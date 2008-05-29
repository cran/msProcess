## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSAlign.q#2 $
## $DateTime: 2008/05/28 14:08:01 $

backMSAlign = function(data){
	
	assign("propData", data, where = 1)
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)


#> getMethodProps("MSAlign")
#	 "MSAlignDataSet"            "MSAlignFUN"          		 "MSAlignMZPrecision"        "MSAlignSNRThreshold"      
#      "MSAlignSaveAs"            "MSAlignMRDWavelet"         "MSAlignMRDLevels"    	 "MSAlignMRDReflect" 
#      "MSAlignPrintObject"       "MSAlignPrintHistory"       "MSAlignPlotResult"        "MSAlignPlotXAxisVariable"  
#      "MSAlignPlotSpectraSubset" "MSAlignPlotSpectraOffset"  "MSAlignImageResult" 		 "MSAlignImageXAxisVariable" 
#      "MSAlignImageSpectraSubset"     

	motherProps = c( "MSAlignFUN", "MSAlignMZPrecision", "MSAlignSNRThreshold", "MSAlignSaveAs" )
	mrdProps = c( "MSAlignMRDWavelet", "MSAlignMRDLevels", "MSAlignMRDReflect" )
	displayProps = c( "MSAlignPrintObject", "MSAlignPrintHistory", "MSAlignPlotResult", "MSAlignPlotXAxisVariable",  
			          "MSAlignPlotSpectraSubset", "MSAlignPlotSpectraOffset", "MSAlignImageResult", 
			          "MSAlignImageXAxisVariable", "MSAlignImageSpectraSubset" )

	allMethodProps = c(motherProps, mrdProps, displayProps)

	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
	}

	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSAlignDataSet", paste(objects(classes = "msSet"), collapse = ", "))
	}	

	## actions based on selecting the data set
	if(activeprop == "MSAlignDataSet"){
		if(exists(cbGetCurrValue(data, "MSAlignDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSAlignSaveAs", 
							  paste(cbGetCurrValue(data, "MSAlignDataSet"), ".align", sep = ""))
			data = cbSetOptionList(data, "MSAlignDataType", paste(names(get(cbGetCurrValue(data, "MSAlignDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSAlignDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
		for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
		}

	}
			
	data
}
