## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/loadMSProps.q#1 $
## $DateTime: 2008/05/15 15:30:09 $

loadMSProps = function()
{
	package.path = .find.package("msProcess")
	prop.path <- "MSProcess.prp"
	info.path <- "MSProcess.fni"
	prop.path <- paste(package.path, ".Prefs", prop.path, sep = "/")
	info.path <- paste(package.path, ".Prefs", info.path, sep = "/")
	guiLoadDefaultObjects("Property", FileName = prop.path)
	guiLoadDefaultObjects("FunctionInfo", FileName = info.path)
}
