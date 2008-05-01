msLaunchExample <- function(x, open=TRUE, run=TRUE, 
		type="R-ex")
{
	# check input arguments
	if (!is.character(x) || length(x) > 1.0)
		stop("x must be a single character string")
	type <- match.arg(lowerCase(type), c("r-ex","bookexams","demo"))
	
	# add extension to file name
	ext <- ".R"
	if (!length(grep(ext, x)))
		x <- paste(x, ext, sep="")
	
	exampleDir <- file.path(.find.package(package="msProcess")[1], type)
	path <- file.path(exampleDir, x)
	if (!file.exists(path))
		stop(path, " does not exist.")
	
	# TODO: use callBrowse() instead?
	if (open)
		guiOpen("Script", FileName=path, Hide=FALSE, Show="Normal", 
				Top="Auto", Left="Auto", Width="Auto", Height="Auto")
	
	if (run)
		source(path)
	
	invisible(NULL)
}
