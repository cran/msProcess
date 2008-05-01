## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/guicreate/addMSGUI.q#9 $
## $DateTime: 2008/04/30 16:28:27 $

addMSGUI = function(){
	
  propPath <- "MSProcessProp.dft"
  infoPath <- "MSProcessFunc.dft"  
  
  statMenuLoc <-
    guiGetPropertyValue("MenuItem",
                        Name = paste(guiGetMenuBar(), "Statistics", sep = "$"),
                        PropName = "Index")
                        
  MSProcessMenuName = "SPlusMenuBar$SplusMSProcess"
 
  guiCreate("MenuItem",
            Name = MSProcessMenuName,
            Type = "Menu", 
            Action = "None",
            MenuItemText = "&msProcess",
            StatusBarText = "Menu for msProcess",
            Index = as.numeric(statMenuLoc) + 1,
            OverWrite = F,
            EnableMenuItem = T)

#################################
####  Menu for demo scripts  ####
#################################

	"menu.path" <- function(...) paste(..., sep="$")
		  
	"createMenu" <- function(menuPath, menuText, Type="MenuItem", ...)
	    invisible(guiCreate("MenuItem", Name=menuPath, Type=Type, MenuItemText=menuText, ...))

	"checkPath" <- function(x,type="dir"){
	    type <- match.arg(type,c("dir","file"))
	    if ((type == "dir" && !is.dir(x)) || (type == "file" && !file.exists(x)))
	      stop(x, " does not exist")
	    invisible(NULL)
	} ## end function def
	
	"createScriptMenus" <- function(x, baseMenuPath, open=TRUE, run=FALSE, type="appexams"){
	    # check data.frame structure
	    if (!is.data.frame(x))
	      stop("Input must be a data.frame")
	    required <- c("brief","filename")
	    if (!all(is.element(required, names(x))))
	      stop("Input data frame must contain the following columns: ", required)
	    
	    for (i in seq(numRows(x))){
	
	      example <- x[i,]
	      fname   <- strip.blanks(example$filename)
	      path    <- paste(baseMenuPath, paste("eg", i, sep=""), sep="$")
	      cmd     <- paste("msLaunchExample(\"", fname, "\", run=", run,
	        ", open=", open, ", type=\"", type, "\")", sep="")
	
	      guiCreate("MenuItem", Name=path, Type="MenuItem", Action="Expression", Command=cmd,
	        MenuItemText=example$brief, StatusBarText=strip.blanks(example$brief))
	    }
	
	    invisible(NULL)
	}  ## end function def
	
	"smartOrder" <- function(x){
		w       <- regexpr("[0-9]+", x)
	    nums    <- as.numeric(substring(x, w, w+attr(w, "match.length")-1))
	    good    <- (!is.na(nums))
	    z       <- rep("", length(x))
	    z[good] <- sprintf("%03d", nums[good])
	    order(sub("[0-9]+", z, x))
	}

	# initialize variables
	chapter <- "msProcess"
	  
	baseMenuPath     <- MSProcessMenuName		# menu.path(MSProcessMenuName, chapter)
	helpMenuPath     <- menu.path(baseMenuPath, "Help")
	demoMenuPath     <- menu.path(baseMenuPath, "Demo") 	        
	exampleMenuPath  <- menu.path(baseMenuPath, "R-ex")
	manualsMenuPath  <- menu.path(baseMenuPath, "Manuals")
	    
	# create file and directory paths
	chapterPath      <- .find.package(package=chapter)[1]
	
	# check existence of file and directory paths
	checkPath(chapterPath)

	# help submenu
	createMenu(helpMenuPath, "&Help", Type="Menu")
	helpcmd <- paste("\"", file.path(chapterPath, paste(chapter,".chm", sep=""), fsep="/"), "\"", sep="")
	helptext <- paste(chapter, " Help", sep="")
	createMenu(menu.path(helpMenuPath,"chm_help"), helptext, Action="Open", StatusBarText=helptext, Command=helpcmd)
	    
	# scan lists
	RexDir   <- file.path(chapterPath, "R-ex")	    
	Rex <- file.listing(dir=RexDir, pattern=".R")
	briefs    <- gsub("_", " ", gsub(".R$", "", gsub("group-","", Rex), ignore.case=T))
	ix        <- smartOrder(briefs)
	Rex <- data.frame(filename=Rex[ix], brief=briefs[ix], stringsAsFactors=FALSE)
	    
	demoDir   <- file.path(chapterPath, "demo")
	demofiles  <- file.listing(dir=demoDir, pattern=".R")
	demoBriefs <- gsub("_", " ", gsub(".R$", "", gsub("group-","", demofiles), ignore.case=T))
	ix         <- smartOrder(demoBriefs)
	demo      <- data.frame(filename=demofiles[ix], brief=demoBriefs[ix], stringsAsFactors=FALSE)
	    
	# demo submenu
	createMenu(demoMenuPath, "&Demo", Type="Menu")   
	createScriptMenus(demo, demoMenuPath, type="demo")
	        
	# examples submenu
	createMenu(exampleMenuPath, "R-ex", Type="Menu")
	createScriptMenus(Rex, exampleMenuPath, type="R-ex")
	    
	# manuals submenu
	createMenu(manualsMenuPath , "&Manual" , Type="Menu")
	helpcmd <- paste("\"", file.path(chapterPath, paste("doc/", chapter,"-manual.pdf", sep=""), fsep="/"), "\"", sep="")
	helptext <- paste(chapter, " Manual", sep="")
	createMenu(menu.path(manualsMenuPath,"pdf_manual"), helptext, Action="Open", 
	       		StatusBarText=helptext, Command=helpcmd)    

#####################################
####  Menu for processing steps  ####
#####################################

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Separator1", sep = "$"),
            Type = "Separator",
            Action = "None")


  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Prepare", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSPrepare",
            ShowDialogOnRun = T,
            MenuItemText = "&Prepare...",
            StatusBarText = "Prepare mass spec data",
			EnableMenuItem = T)

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Separator2", sep = "$"),
            Type = "Separator",
            Action = "None")

  guiCreate( "MenuItem", 
  		    Name = paste(MSProcessMenuName, "Denoise", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Noise Reduction...",
		    StatusBarText = "Denoise mass spec data",
		    Command = "menuMSDenoise",
		    ShowDialogOnRun = T,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

  guiCreate( "MenuItem", 
  		    Name = paste(MSProcessMenuName, "Noise", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Noise Estimation...",
		    StatusBarText = "Local noise estimation",
		    Command = "menuMSNoise",
		    ShowDialogOnRun = T,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Detrend", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Detrend...",
		    StatusBarText = "Baseline correction",
		    Command = "menuMSDetrend",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)
		    		    		    		    		    		    
 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Normalize", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "None",
		    MenuItemText = "Normalize...",
		    StatusBarText = "Intensity Normalization",
		    Command = "",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Peak", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "None",
		    MenuItemText = "Peak Detection...",
		    StatusBarText = "Peak detection",
		    Command = "",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Align", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "None",
		    MenuItemText = "Peak Alignment...",
		    StatusBarText = "Peak alignment",
		    Command = "",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)		    

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Quantify", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "None",
		    MenuItemText = "Peak Quantification...",
		    StatusBarText = "Peak quantification",
		    Command = "",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)		    

###################
####  Prepare  ####
###################

  guiCreate("Property",
            Name = "MSPrepDataSet",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "&Data Set:",
            DialogControl = "Combo Box",
            OptionList = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = T,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSPrepDataGroup",
            Type = "Group",
            DialogPrompt = "Data",
            PropertyList = "MSPrepDataSet",
            SavePathName = propPath)

  guiCreate("Property",
            Name = "MSPrepMassMin",
            Type = "Normal",
            DefaultValue = "1500",
            ParentProperty = "",
            DialogPrompt = "Mass Minimum:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
            
  guiCreate("Property",
            Name = "MSPrepTransform",
            Type = "Normal",
            DefaultValue = "<None>",
            ParentProperty = "",
            DialogPrompt = "Transformation:",
            DialogControl = "Combo Box",
            OptionList = "<None>, log, log10, sqrt, cubert",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSPrepOptionsGroup",
            Type = "Group",
            DialogPrompt = "Preparation Options",
            PropertyList = paste(c("MSPrepMassMin",
            						"MSPrepTransform"),
            					  collapse=", "),
            SavePathName = propPath)


  guiCreate("Property",
            Name = "MSPrepDataName",
            Type = "Normal",
            DefaultValue = "",
            ParentProperty = "",
            DialogPrompt = "Data Name:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSPrepPrintObject",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print Object Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
            
           
  guiCreate("Property",
            Name = "MSPrepOutputOptionsGroup",
            Type = "Group",
            DialogPrompt = "Output Options",
            PropertyList = paste(c( "MSPrepDataName",
            						"MSPrepPrintObject"),
            					  collapse=", "),
            SavePathName = propPath)
            
                       
  guiCreate("Property",
            Name = "MSPrepSaveAs",
            Type = "Normal",
            DefaultValue = "",
            ParentProperty = "",
            DialogPrompt = "Save As:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)
            
  guiCreate("Property",
            Name = "MSPrepSaveAsGroup",
            Type = "Group",
            DialogPrompt = "Save msSet Object",
            PropertyList = "MSPrepSaveAs",
            SavePathName = propPath)

####################################
####  FunctionInfo for Prepare  ####
####################################

  guiCreate("FunctionInfo",
            Name = "menuMSPrepare",
            Function = "menuMSPrepare",
            DialogHeader = "Prepare msSet",
            StatusString = "Create an msSet Object from an msList Object",
            PropertyList =
            	paste(c("SPropInvisibleReturnObject",
            			"MSPrepDataGroup",
                	    "MSPrepOptionsGroup",
                	    "MSPrepOutputOptionsGroup", 
                	    "MSPrepSaveAsGroup"),
                  	  collapse = ", "),
            ArgumentList =
            	paste(c("#0=SPropInvisibleReturnObject", #
                    	"#1=MSPrepDataSet",    #
                    	"#2=MSPrepMassMin",	  	#
                    	"#3=MSPrepTransform", 	#
                    	"#4=MSPrepDataName",  #
                    	"#5=MSPrepPrintObject",
                    	"#6=MSPrepSaveAs"),	#
                  	 collapse = ", "),
            ArgumentClassList = "",
            PromptList = "",
            DefaultValueList = "",
            CallBackFunction = "backMSPrepare",
            HelpCommand = "",
            WriteArgNames = T,
            Display = F,
            SavePathName = infoPath)
 
 
####################
####  Denoise  #####
####################
 
 
  guiCreate("Property",
            Name = "MSDenoiseDataSet",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "Data Set:",
            DialogControl = "Combo Box",
            OptionList = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = T,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
           
  guiCreate("Property",
            Name = "MSDenoiseDataType",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "Select Data:",
            DialogControl = "List Box",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseDataGroup",
            Type = "Group",
            DialogPrompt = "Data Selection",
            PropertyList = paste(c( "MSDenoiseDataSet",
            						"MSDenoiseDataType"),
            					  collapse=", "),
            SavePathName = propPath)

  guiCreate("Property",
            Name = "MSDenoiseFUN",
            Type = "Normal",
            DefaultValue = "wavelet",
            ParentProperty = "",
            DialogPrompt = "Method:",
            DialogControl = "Combo Box",
            OptionList = "smooth, mrd, wavelet",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseMargin",
            Type = "Normal",
            DefaultValue = "2",
            ParentProperty = "",
            DialogPrompt = "Margin:",
            DialogControl = "List Box",
            OptionList = "1, 2",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = F,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSDenoiseMethodGroup",
            Type = "Group",
            DialogPrompt = "Method Selection",
            PropertyList = paste(c("MSDenoiseFUN",
            					   "MSDenoiseMargin"),
            					  collapse=", "),
            SavePathName = propPath)
 
            
  guiCreate("Property",
            Name = "MSDenoiseEventLabel",
            Type = "Normal",
            DefaultValue = "Denoising",
            ParentProperty = "",
            DialogPrompt = "Process Label:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F) 


  guiCreate("Property",
            Name = "MSDenoiseAttachNoise",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Attach Noise",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = F,
            IsList = F,
            Disable = F) 
            

  guiCreate("Property",
            Name = "MSDenoisePrintObject",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print Object Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
            
  guiCreate("Property",
            Name = "MSDenoisePrintHistory",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print History Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)            
            
  guiCreate("Property",
            Name = "MSDenoiseOptionsGroup",
            Type = "Group",
            DialogPrompt = "Output Options",
            PropertyList = paste(c( "MSDenoiseEventLabel",
            						"MSDenoiseAttachNoise",
            				 		"MSDenoisePrintObject",
            				 		"MSDenoisePrintHistory"),
            				 	 collapse = ", "), 
            SavePathName = propPath)
            
            
  guiCreate("Property",
            Name = "MSDenoiseSaveAs",
            Type = "Normal",
            DefaultValue = "",
            ParentProperty = "",
            DialogPrompt = "Save As:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)
           
  guiCreate("Property",
            Name = "MSDenoiseSaveAsGroup",
            Type = "Group",
            DialogPrompt = "Save Denoised msSet Object",
            PropertyList = "MSDenoiseSaveAs",
            SavePathName = propPath)


  guiCreate("Property",
            Name = "MSDenoiseMotherTab",
            Type = "Page", 
            DialogPrompt = "Data/Methods",
            PropertyList =paste(c(  "MSDenoiseDataGroup",
                					"MSDenoiseMethodGroup",
                					"MSDenoiseOptionsGroup",
                					"MSDenoiseSaveAsGroup"),
                				collapse = ", "),
            SavePathName = propPath)
            
         
### Smooth Tab
  guiCreate("Property",
            Name = "MSDenoiseSmoothTwiceit",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Twice it",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F) 

  guiCreate("Property",
            Name = "MSDenoiseSmoothOptionsGroup",
            Type = "Group",
            DialogPrompt = "Smoothing Options",
            PropertyList = paste(c("MSDenoiseSmoothTwiceit"),
            					collapse = ", "),
            SavePathName = propPath)
                        
  guiCreate("Property",
            Name = "MSDenoiseSmoothTab",
            Type = "Page", 
            DialogPrompt = "Smooth",
            PropertyList = "MSDenoiseSmoothOptionsGroup",
            SavePathName = propPath)           

### MRD Tab

  guiCreate("Property",
            Name = "MSDenoiseMRDWavelet",
            Type = "Normal",
            DefaultValue = "s8",
            ParentProperty = "",
            DialogPrompt = "Wavelet:",
            DialogControl = "Combo Box",
            OptionList = paste(c("haar", "d2", "d4", "d6", "d8", "d10", "d12", "d14",
            					 "d16", "d18", "d20", "s2","s4", "s6", "s8", "s10", "s12", 
            					 "s14", "s16", "s18", "s20", "l2","l4", "l6", "l14", "l18", 
            					 "l20", "c6", "c12", "c18", "c24", "c30"), collapse = ", "),
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseMRDXForm",
            Type = "Normal",
            DefaultValue = "modwt",
            ParentProperty = "",
            DialogPrompt = "Wavelet Transform:",
            DialogControl = "List Box",
            OptionList = "dwt, modwt",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseMRDLevels",
            Type = "Normal",
            DefaultValue = "1",
            ParentProperty = "",
            DialogPrompt = "Levels:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F) 
            
  guiCreate("Property",
            Name = "MSDenoiseMRDReflect",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Reflect",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = F,
            IsList = F,
            Disable = F)

            
  guiCreate("Property",
            Name = "MSDenoiseMRDKeepSmooth",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Keep Smooth",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = F,
            IsList = F,
            Disable = F)

                                    
  guiCreate("Property",
            Name = "MSDenoiseMRDKeepDetails",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Keep Details",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseMRDSpecGroup",
            Type = "Group",
            DialogPrompt = "Wavelet Specification",
            PropertyList = paste(c("MSDenoiseMRDWavelet",
            						"MSDenoiseMRDXForm",
            						"MSDenoiseMRDLevels"),
            					collapse = ", "),
            SavePathName = propPath)
            
  guiCreate("Property",
            Name = "MSDenoiseMRDOptionsGroup",
            Type = "Group",
            DialogPrompt = "Wavelet Options",
            PropertyList = paste(c("MSDenoiseMRDReflect",
            					   "MSDenoiseMRDKeepSmooth",
            					   "MSDenoiseMRDKeepDetails"),
            					collapse = ", "),
            SavePathName = propPath)    
            
                                            
  guiCreate("Property",
            Name = "MSDenoiseMRDTab",
            Type = "Page", 
            DialogPrompt = "MRD",
            PropertyList =paste(c("MSDenoiseMRDSpecGroup",
            					  "MSDenoiseMRDOptionsGroup"),
            	  				 collapse = ", "), 
            SavePathName = propPath)           

### Wavlet Options
  guiCreate("Property",
            Name = "MSDenoiseWaveletWavelet",
            Type = "Normal",
            DefaultValue = "s8",
            ParentProperty = "",
            DialogPrompt = "Wavelet:",
            DialogControl = "Combo Box",
            OptionList = paste(c("haar", "d2", "d4", "d6", "d8", "d10", "d12", "d14",
            					 "d16", "d18", "d20", "s2","s4", "s6", "s8", "s10", "s12", 
            					 "s14", "s16", "s18", "s20", "l2","l4", "l6", "l14", "l18", 
            					 "l20", "c6", "c12", "c18", "c24", "c30"), collapse = ", "),
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseWaveletXForm",
            Type = "Normal",
            DefaultValue = "modwt",
            ParentProperty = "",
            DialogPrompt = "Wavelet Transform:",
            DialogControl = "List Box",
            OptionList = "dwt, modwt",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseWaveletNLevels",
            Type = "Normal",
            DefaultValue = "",  
            ParentProperty = "",
            DialogPrompt = "No. Levels:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F) 
            
  guiCreate("Property",
            Name = "MSDenoiseWaveletReflect",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Reflect",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = F,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSDenoiseWaveletShrinkFun",
            Type = "Normal",
            DefaultValue = "hard",
            ParentProperty = "",
            DialogPrompt = "Shrinkage Fun:",
            DialogControl = "List Box",
            OptionList = "hard, mid, soft",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDenoiseWaveletThreshFun",
            Type = "Normal",
            DefaultValue = "universal",
            ParentProperty = "",
            DialogPrompt = "Threshold Fun:",
            DialogControl = "List Box",
            OptionList = "adaptive, minimax, universal",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)           


  guiCreate("Property",
            Name = "MSDenoiseWaveletThreshScale",
            Type = "Normal",
            DefaultValue = "1",  
            ParentProperty = "",
            DialogPrompt = "Threshold Scale:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)            
 
  guiCreate("Property",
            Name = "MSDenoiseWaveletNoiseVar",
            Type = "Normal",
            DefaultValue = "",  
            ParentProperty = "",
            DialogPrompt = "Noise Variance:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)            
                       
  guiCreate("Property",
            Name = "MSDenoiseWaveletAssignAttr",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Assign Attrib",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSDenoiseWaveletSpecGroup",
            Type = "Group",
            DialogPrompt = "Wavelet Specification",
            PropertyList = paste(c("MSDenoiseWaveletWavelet",
            						"MSDenoiseWaveletXForm",
            						"MSDenoiseWaveletNLevels"),
            					collapse = ", "),
            SavePathName = propPath)
            
  guiCreate("Property",
            Name = "MSDenoiseWaveletOptionsGroup",
            Type = "Group",
            DialogPrompt = "Wavelet Options",
            PropertyList = paste(c("MSDenoiseWaveletShrinkFun",
            					  "MSDenoiseWaveletThreshFun",
            					  "MSDenoiseWaveletThreshScale", 
            					  "MSDenoiseWaveletNoiseVar",
            					  "MSDenoiseWaveletReflect",
            					  "MSDenoiseWaveletAssignAttr"),
            					collapse = ", "),
            SavePathName = propPath)            
                        
  guiCreate("Property",
            Name = "MSDenoiseWaveletTab",
            Type = "Page", 
            DialogPrompt = "Wavelet",
            PropertyList =paste(c("MSDenoiseWaveletSpecGroup",
            					  "MSDenoiseWaveletOptionsGroup"),
            	  				 collapse = ", "), 
            SavePathName = propPath)       



################################################
####  FunctionInfo Object for menuMSDenoise ####
################################################

           				 		
  guiCreate("FunctionInfo",
            Name = "menuMSDenoise",
            Function = "menuMSDenoise",
            DialogHeader = "Denoise msSet Object",
            StatusString = "Denoise an msSet Object",
            PropertyList =
            	paste(c("SPropInvisibleReturnObject",
            			"MSDenoiseMotherTab",
            			"MSDenoiseSmoothTab",
            			"MSDenoiseMRDTab", 
            			"MSDenoiseWaveletTab"),
            		   collapse = ", "),
            ArgumentList =
            	paste(c("#0=SPropInvisibleReturnObject", 
                    	"#1=MSDenoiseDataSet",
                    	"#2=MSDenoiseDataType",				
                    	"#3=MSDenoiseFUN",  		
                    	"#4=MSDenoiseMargin",	  	
                    	"#5=MSDenoiseAttachNoise",  
                    	"#6=MSDenoiseEventLabel",	
                    	"#7=MSDenoisePrintObject",	
                    	"#8=MSDenoisePrintHistory",	
                    	"#9=MSDenoiseSmoothTwiceit",		
                    	"#10=MSDenoiseWaveletAssignAttr",	
                    	"#11=MSDenoiseWaveletNLevels",		
                    	"#12=MSDenoiseWaveletShrinkFun", 
                    	"#13=MSDenoiseWaveletThreshFun", 	
                    	"#14=MSDenoiseWaveletThreshScale",	
                    	"#15=MSDenoiseWaveletNoiseVar", 	
                    	"#16=MSDenoiseWaveletWavelet", 	
                    	"#17=MSDenoiseWaveletXForm", 	
                    	"#18=MSDenoiseWaveletReflect",	
                    	"#19=MSDenoiseMRDWavelet",	
                    	"#20=MSDenoiseMRDXForm", 	
                    	"#21=MSDenoiseMRDReflect", 	
                    	"#22=MSDenoiseMRDLevels", 	
                    	"#23=MSDenoiseMRDKeepDetails",	
                    	"#24=MSDenoiseMRDKeepSmooth",
                    	"#25=MSDenoiseSaveAs"),	
                  	 collapse = ", "),
            ArgumentClassList = "",
            PromptList = "",
            DefaultValueList = "",
            CallBackFunction = "backMSDenoise",
            HelpCommand = "",
            WriteArgNames = T,
            Display = F,
            SavePathName = infoPath)



#############################
####  Noise Estimation  #####
#############################
 
  guiCreate("Property",
            Name = "MSNoiseDataSet",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "Data Set:",
            DialogControl = "Combo Box",
            OptionList = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = T,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
           
  guiCreate("Property",
            Name = "MSNoiseDataType",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "Select Data:",
            DialogControl = "List Box",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSNoiseDataGroup",
            Type = "Group",
            DialogPrompt = "Data Selection",
            PropertyList = paste(c( "MSNoiseDataSet",
            						"MSNoiseDataType"),
            					  collapse=", "),
            SavePathName = propPath)


  guiCreate("Property",
            Name = "MSNoiseFUN",
            Type = "Normal",
            DefaultValue = "spline",
            ParentProperty = "",
            DialogPrompt = "Method:",
            DialogControl = "Combo Box",
            OptionList = "mean, ksmooth, loess, spline, supsmu",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSNoisePre",
            Type = "Normal",
            DefaultValue = "abs",
            ParentProperty = "",
            DialogPrompt = "Preprocess:",
            DialogControl = "Combo Box",
            OptionList = "abs, log, log10, sqrt",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

                       
  guiCreate("Property",
            Name = "MSNoiseMargin",
            Type = "Normal",
            DefaultValue = "2",
            ParentProperty = "",
            DialogPrompt = "Margin:",
            DialogControl = "List Box",
            OptionList = "1, 2",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSNoiseMethodGroup",
            Type = "Group",
            DialogPrompt = "Method Selection",
            PropertyList = paste(c("MSNoiseFUN",
            					   "MSNoisePre",
            					   "MSNoiseMargin"),
            					  collapse=", "),
            SavePathName = propPath)
 
  guiCreate("Property",
            Name = "MSNoiseEventLabel",
            Type = "Normal",
            DefaultValue = "Local Noise Estimation",
            ParentProperty = "",
            DialogPrompt = "Process Label:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F) 

           
  guiCreate("Property",
            Name = "MSNoiseDetachNoise",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Detach Noise",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F) 
            

  guiCreate("Property",
            Name = "MSNoisePrintObject",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print Object Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
            
  guiCreate("Property",
            Name = "MSNoisePrintHistory",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print History Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)            
            
  guiCreate("Property",
            Name = "MSNoiseOptionsGroup",
            Type = "Group",
            DialogPrompt = "Output Options",
            PropertyList = paste(c( "MSNoiseEventLabel",
            						"MSNoiseDetachNoise",
            				 		"MSNoisePrintObject",
            				 		"MSNoisePrintHistory"),
            				 	 collapse = ", "), 
            SavePathName = propPath)


  guiCreate("Property",
            Name = "MSNoiseSaveAs",
            Type = "Normal",
            DefaultValue = "",
            ParentProperty = "",
            DialogPrompt = "Save As:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)
           
  guiCreate("Property",
            Name = "MSNoiseSaveAsGroup",
            Type = "Group",
            DialogPrompt = "Save msSet Object with Estimated Noise",
            PropertyList = "MSNoiseSaveAs",
            SavePathName = propPath)
            
           

##################################
####  FunctionInfo for Noise  ####
##################################  

  guiCreate("FunctionInfo",
            Name = "menuMSNoise",
            Function = "menuMSNoise",
            DialogHeader = "Noise Estimation",
            StatusString = "Noise estimation for an msSet object",
            PropertyList =
            	paste(c("SPropInvisibleReturnObject",
            			"MSNoiseDataGroup",
            			"MSNoiseMethodGroup",
            			"MSNoiseOptionsGroup",
            			"MSNoiseSaveAsGroup"),
            		   collapse = ", "),
            ArgumentList =
            	paste(c("#0=SPropInvisibleReturnObject", 
                    	"#1=MSNoiseDataSet",
                    	"#2=MSNoiseDataType",				
                    	"#3=MSNoiseFUN", 
                    	"#4=MSNoisePre", 		
                    	"#5=MSNoiseMargin",	
                    	"#6=MSNoiseEventLabel",   	
                    	"#7=MSNoiseDetachNoise",
                    	"#8=MSNoisePrintObject",
                    	"#9=MSNoisePrintHistory",
                    	"#10=MSNoiseSaveAs"),
                      collapse = ", "),
            ArgumentClassList = "",
            PromptList = "",
            DefaultValueList = "",
            CallBackFunction = "backMSNoise",
            HelpCommand = "",
            WriteArgNames = T,
            Display = F,
            SavePathName = infoPath)                      

###################
####  Detrend  ####
###################

  guiCreate("Property",
            Name = "MSDetrendDataSet",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "Data Set:",
            DialogControl = "Combo Box",
            OptionList = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = T,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
           
  guiCreate("Property",
            Name = "MSDetrendDataType",
            Type = "Normal",
            DefaultValue = "",
            DialogPrompt = "Select Data:",
            DialogControl = "List Box",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDetrendDataGroup",
            Type = "Group",
            DialogPrompt = "Data Selection",
            PropertyList = paste(c( "MSDetrendDataSet",
            						"MSDetrendDataType"),
            					  collapse=", "),
            SavePathName = propPath)


  guiCreate("Property",
            Name = "MSDetrendFUN",
            Type = "Normal",
            DefaultValue = "loess",
            ParentProperty = "",
            DialogPrompt = "Method:",
            DialogControl = "Combo Box",
            OptionList = "approx, monotone, mrd, loess, spline, supsmu",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

  guiCreate("Property",
            Name = "MSDetrendPre",
            Type = "Normal",
            DefaultValue = "<None>",
            ParentProperty = "",
            DialogPrompt = "Preprocess:",
            DialogControl = "Combo Box",
            OptionList = "<None>, abs, log, log10, sqrt",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)

                       
  guiCreate("Property",
            Name = "MSDetrendMargin",
            Type = "Normal",
            DefaultValue = "2",
            ParentProperty = "",
            DialogPrompt = "Margin:",
            DialogControl = "List Box",
            OptionList = "1, 2",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)


  guiCreate("Property",
            Name = "MSDetrendMethodGroup",
            Type = "Group",
            DialogPrompt = "Method Selection",
            PropertyList = paste(c("MSDetrendFUN",
            					   "MSDetrendPre",
            					   "MSDetrendMargin"),
            					  collapse=", "),
            SavePathName = propPath)
 
  guiCreate("Property",
            Name = "MSDetrendEventLabel",
            Type = "Normal",
            DefaultValue = "Baseline Correction",
            ParentProperty = "",
            DialogPrompt = "Process Label:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F) 

           
  guiCreate("Property",
            Name = "MSDetrendAttachBaseline",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Add Baseline",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F) 
            

  guiCreate("Property",
            Name = "MSDetrendPrintObject",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print Object Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)
            
  guiCreate("Property",
            Name = "MSDetrendPrintHistory",
            Type = "Normal",
            DefaultValue = T,
            ParentProperty = "",
            DialogPrompt = "Print History Information",
            DialogControl = "Check Box",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = F,
            NoQuotes = T,
            IsList = F,
            Disable = F)            
            
  guiCreate("Property",
            Name = "MSDetrendOptionsGroup",
            Type = "Group",
            DialogPrompt = "Output Options",
            PropertyList = paste(c( "MSDetrendEventLabel",
            						"MSDetrendAttachBaseline",
            				 		"MSDetrendPrintObject",
            				 		"MSDetrendPrintHistory"),
            				 	 collapse = ", "), 
            SavePathName = propPath)


  guiCreate("Property",
            Name = "MSDetrendSaveAs",
            Type = "Normal",
            DefaultValue = "",
            ParentProperty = "",
            DialogPrompt = "Save As:",
            DialogControl = "String",
            OptionList = "",
            CopyFrom = "",
            HelpString = "",
            SavePathName = propPath,
            IsRequired = F,
            UseQuotes = T,
            NoQuotes = F,
            IsList = F,
            Disable = F)
           
  guiCreate("Property",
            Name = "MSDetrendSaveAsGroup",
            Type = "Group",
            DialogPrompt = "Save Detrended msSet Object",
            PropertyList = "MSDetrendSaveAs",
            SavePathName = propPath)
            
           
####################################
####  FunctionInfo for Detrend  ####
####################################  

  guiCreate("FunctionInfo",
            Name = "menuMSDetrend",
            Function = "menuMSDetrend",
            DialogHeader = "Baseline Correction",
            StatusString = "Baseline correction for an msSet object",
            PropertyList =
            	paste(c("SPropInvisibleReturnObject",
            			"MSDetrendDataGroup",
            			"MSDetrendMethodGroup",
            			"MSDetrendOptionsGroup",
            			"MSDetrendSaveAsGroup"),
            		   collapse = ", "),
            ArgumentList =
            	paste(c("#0=SPropInvisibleReturnObject", 
                    	"#1=MSDetrendDataSet",
                    	"#2=MSDetrendDataType",				
                    	"#3=MSDetrendFUN", 
                    	"#4=MSDetrendPre", 		
                    	"#5=MSDetrendMargin",	
                    	"#6=MSDetrendEventLabel",   	
                    	"#7=MSDetrendAttachBaseline",
                    	"#8=MSDetrendPrintObject",
                    	"#9=MSDetrendPrintHistory",
                    	"#10=MSDetrendSaveAs"),
                      collapse = ", "),
            ArgumentClassList = "",
            PromptList = "",
            DefaultValueList = "",
            CallBackFunction = "backMSDetrend",
            HelpCommand = "",
            WriteArgNames = T,
            Display = F,
            SavePathName = infoPath)                      





invisible()
}
