## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/loadMSGUIAll.q#1 $
## $DateTime: 2008/04/29 16:30:15 $

guiPath = "d:\\Research\\packages_s\\msProcess\\swingui"
loadPath = "d:\\Research\\Proteome"

loadMSGUIAll = function(){
	removeMSGUI()
	
	funs.files = files.in.dir(paste(guiPath, "R", sep = "\\"))
	funs.files = funs.files[grep("q$", funs.files)]
	for(i in funs.files){
		source(paste(guiPath, "R", i, sep = "\\"))
	}
	gui.files = files.in.dir(paste(guiPath, "guicreate", sep = "\\"))
	gui.files = gui.files[grep("q$", gui.files)]
	for(i in gui.files){
		source(paste(guiPath, "guicreate", i, sep = "\\"))
	}
	synchronize(1)
	addMSGUI()		
}

