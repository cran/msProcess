## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/getMethodProps.q#2 $
## $DateTime: 2008/05/07 18:16:12 $


getMethodProps = function(pattern){
	item = guiGetObjectNames("Property")
	item[grep(pattern, item)]
}
