## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSDenoise.q#12 $
## $DateTime: 2008/05/13 14:18:36 $

backMSDenoise = function(data){
	
	assign("propData", data, where = 1)
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)


	
	## set startup properties

	#> getMethodProps("MSDenoiseWavelet")
	# [1] "MSDenoiseWaveletWavelet"      "MSDenoiseWaveletXForm"        "MSDenoiseWaveletNLevels"     
	# [4] "MSDenoiseWaveletReflect"      "MSDenoiseWaveletShrinkFun"    "MSDenoiseWaveletThreshFun"   
	# [7] "MSDenoiseWaveletThreshScale"  "MSDenoiseWaveletNoiseVar"     "MSDenoiseWaveletAssignAttr"  
	#[10] "MSDenoiseWaveletSpecGroup"    "MSDenoiseWaveletOptionsGroup" "MSDenoiseWaveletTab"         
	#> getMethodProps("MSDenoiseSmooth")
	#[1] "MSDenoiseSmoothTwiceit"      "MSDenoiseSmoothOptionsGroup" "MSDenoiseSmoothTab"         
	#> getMethodProps("MSDenoiseMRD")
	#[1] "MSDenoiseMRDWavelet"      "MSDenoiseMRDXForm"        "MSDenoiseMRDLevels"       "MSDenoiseMRDReflect"     
	#[5] "MSDenoiseMRDKeepSmooth"   "MSDenoiseMRDKeepDetails"  "MSDenoiseMRDSpecGroup"    "MSDenoiseMRDOptionsGroup"
	#[9] "MSDenoiseMRDTab"        
	motherProps = c("MSDenoiseFUN", "MSDenoiseEventLabel", "MSDenoiseAttachNoise", 
					"MSDenoiseSaveAs")      
	smoothProps = "MSDenoiseSmoothTwiceit"
	mrdProps = c( "MSDenoiseMRDWavelet", "MSDenoiseMRDXForm", "MSDenoiseMRDLevels", "MSDenoiseMRDReflect",
	              "MSDenoiseMRDKeepSmooth", "MSDenoiseMRDKeepDetails") 	
	waveletProps = c("MSDenoiseWaveletWavelet", "MSDenoiseWaveletXForm", "MSDenoiseWaveletNLevels", 
					 "MSDenoiseWaveletReflect", "MSDenoiseWaveletShrinkFun", "MSDenoiseWaveletThreshFun",  
					 "MSDenoiseWaveletThreshScale", "MSDenoiseWaveletNoiseVar", "MSDenoiseWaveletAssignAttr") 
	displayProps = c("MSDenoisePrintObject", "MSDenoisePrintHistory", "MSDenoisePlotResult", "MSDenoisePlotXAxisVariable",
            		 "MSDenoisePlotSpectraSubset", "MSDenoisePlotSpectraOffset", "MSDenoiseImageResult", 
            		 "MSDenoiseImageXAxisVariable", "MSDenoiseImageSpectraSubset")
	allMethodProps = c(motherProps, smoothProps, mrdProps, waveletProps, displayProps)
	
	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
	}
	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSDenoiseDataSet", paste(objects(classes = "msSet"), collapse = ", "))
	}
	## actions based on selecting the data set
	if(activeprop == "MSDenoiseDataSet"){
		if(exists(cbGetCurrValue(data, "MSDenoiseDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSDenoiseSaveAs", 
							  paste(cbGetCurrValue(data, "MSDenoiseDataSet"), ".denoise", sep = ""))
			data = cbSetOptionList(data, "MSDenoiseDataType", paste(names(get(cbGetCurrValue(data, "MSDenoiseDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSDenoiseDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
		for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
		}
		for(i in waveletProps){
				data = cbSetEnableFlag(data, i, T)
		}
		data = cbSetCurrValue(data, "MSDenoiseWaveletNLevels", 
							 floor(logb(numRows(get(cbGetCurrValue(data, "MSDenoiseDataSet"))[["intensity"]]), 2)))
	}

	## actions based on selecting the data column
#	if(activeprop == "MSDenoiseDataType"){
#			for(i in c(motherProps, displayProps)){
#				data = cbSetEnableFlag(data, i, T)
#			}
#			for(i in waveletProps){
#				data = cbSetEnableFlag(data, i, T)
#			}
#			data = cbSetCurrValue(data, "MSDenoiseWaveletNLevels", 
#								  floor(logb(numRows(get(cbGetCurrValue(data, "MSDenoiseDataSet"))[[
#								  								cbGetCurrValue(data, "MSDenoiseDataType")]]), 2)))
#	}

	## actions based on selecting the method
	if(activeprop == "MSDenoiseFUN"){
		method = cbGetCurrValue(data, "MSDenoiseFUN")
	    switch(method, 
	    	"smooth" ={
				for(i in smoothProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(mrdProps, waveletProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},  
			"mrd" = {
				for(i in mrdProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(smoothProps, waveletProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"wavelet" = {
				for(i in waveletProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(smoothProps, mrdProps)){
					data = cbSetEnableFlag(data, i, F)
				}
				if(cbGetCurrValue(data, "MSDenoiseWaveletXForm") == "modwt"){
					data = cbSetOptionList(data, "MSDenoiseWaveletThreshFun", "universal")
					data = cbSetCurrValue(data, "MSDenoiseWaveletThreshFun", "universal")
				} else {
					data = cbSetOptionList(data, "MSDenoiseWaveletThreshFun", 
											paste(c("adaptive", "minimax", "universal"), collapse = ", "))
				}
			})	
	}
	if(activeprop == "MSDenoiseWaveletXForm"){
		if(cbGetCurrValue(data, "MSDenoiseWaveletXForm") == "modwt"){
			data = cbSetOptionList(data, "MSDenoiseWaveletThreshFun", "universal")
			data = cbSetCurrValue(data, "MSDenoiseWaveletThreshFun", "universal")
		} else {
			data = cbSetOptionList(data, "MSDenoiseWaveletThreshFun", 
								   paste(c("adaptive", "minimax", "universal"), collapse = ", "))			
		}
	}
  data
}
