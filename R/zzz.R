########################################################
## msProcess package attachment functions
##
##  .First.lib
##
########################################################

if (!is.R()) {

  ###
  # .First.lib
  ###

  .First.lib <- function(library, section)
  {
	  # define local functions
	  "menu.path" <- function(...) paste(..., sep="$")
	    
	  "createMenu" <- function(menuPath, menuText, Type="MenuItem", ...)
	    invisible(guiCreate("MenuItem", Name=menuPath, Type=Type, MenuItemText=menuText, ...))
	    
	  "readFormattedTable" <- function(path, splits, sep="|", stringsAsFactors=FALSE){
	     z <- scan(path,sep=sep)
	     z <- z[nchar(z) > 0]
	     data.frame(split(z,splits),stringsAsFactors=stringsAsFactors)
	  }
	
	  "checkPath" <- function(x,type="dir"){
	    type <- match.arg(type,c("dir","file"))
	    if ((type == "dir" && !is.dir(x)) || (type == "file" && !file.exists(x)))
	      stop(x, " does not exist")
	    invisible(NULL)
	  }
	
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
	  }
	
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
	
	  # create GUI menus only if supported S-PLUS GUI is available
	  if (is.ui.app("s+gui")){
	  	
	    # create menu paths
	    baseMenuPath     <- menu.path("$$SPlusMenuBar", chapter)
	    helpMenuPath     <- menu.path(baseMenuPath, "Help")
	    demoMenuPath    <- menu.path(baseMenuPath, "Demo") 	        
	    exampleMenuPath  <- menu.path(baseMenuPath, "R-ex")
	    manualsMenuPath  <- menu.path(baseMenuPath, "Manuals")
	    
	    # create file and directory paths
	    chapterPath      <- .find.package(package=chapter)[1]
	
	    # check existence of file and directory paths
	    checkPath(chapterPath)
	
	    # remove any existing related GUI menus
	    if (is.element(baseMenuPath, guiGetObjectNames("MenuItem")))
	      guiRemove("MenuItem", Name=baseMenuPath)
	
	    ###
	    # Menus
	    ###
	
	    # primary chapter menu
	    createMenu(baseMenuPath, chapter, Type="Menu")
	
	    # help submenu
	    createMenu(helpMenuPath, "&Help", Type="Menu")
	    helpcmd <- paste("\"",
	      file.path(chapterPath, paste(chapter,".chm", sep=""), fsep="/"),
	      "\"", sep="")
	    helptext <- paste(chapter, " Help", sep="")
	    createMenu(menu.path(helpMenuPath,"chm_help"), helptext, Action="Open", 
	      StatusBarText=helptext, Command=helpcmd)
	    
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
	    createMenu(demoMenuPath, "&Demo" , Type="Menu")   
	    createScriptMenus(demo, demoMenuPath, type="demo")
	        
	    # examples submenu
	    createMenu(exampleMenuPath, "R-ex", Type="Menu")
	    createScriptMenus(Rex, exampleMenuPath, type="R-ex")
	    
	    # manuals submenu
	    createMenu(manualsMenuPath , "&Manual" , Type="Menu")
	    helpcmd <- paste("\"",
	      file.path(chapterPath, paste("doc/", chapter,"-manual.pdf", sep=""), fsep="/"),
	      "\"", sep="")
	    helptext <- paste(chapter, " Manual", sep="")
	    createMenu(menu.path(manualsMenuPath,"pdf_manual"), helptext, Action="Open", 
	      StatusBarText=helptext, Command=helpcmd)    
	  }

		# DependsSplus in DESCRIPTION file does not work for S-PLUS version < '8.1'
		if (require(pkgutils) && getSversion() < '8.1') {
	    ## load required S-PLUS base libraries
	
	    # robust library
	    robust.pos <- attached.where("robust", nomatch=0)
	    if (!robust.pos) {
	      cat("Loading required library: robust.\n")
	      library("robust", first=TRUE)
	    }
	
	    # SPXML library
	    SPXML.pos <- attached.where("SPXML", nomatch=0)
	    if (!SPXML.pos) {
	      cat("Loading required library: SPXML.\n")
	      library("SPXML")
	    }
	
	    ## load required S-PLUS CSAN packages and install if necessary
	    
	    # create a temporary installation directory for install.packages() calls.
	    # setting destdir explicitly in install.packages() avoids a message printed 
	    # to the command line defining where the temporary installation took place.
	    tmpdir <- file.path(tempfile("dir"), "downloaded_packages")
	    if(!file.exists(tmpdir) && !dir.create(tmpdir, recursive=T))
	      stop(sprintf("Unable to create temporary directory '%s'", tmpdir))
	
	    "quietRequire" <- function(package){
	      if (!is.character(package) || length(package) > 1)
	        stop("package must be a scalar character string")
	      package <- as.character(substitute(package))
	      val <- try(library(package, character.only=T))
	      return(!inherits(val, "Error"))
	    }
	
	    # pkgutils
	    pkgutils.pos <- attached.where("pkgutils", nomatch=0)
	    if (!pkgutils.pos) {
	      dscfile <- system.file(package="pkgutils", "DESCRIPTION")
	      if (!file.exists(dscfile)) {
	        cat("Updating required library: pkgutils\n")
	        install.pkgutils(update=T)
	      }
	
	      cat("Loading required library: pkgutils.\n")
	      library("pkgutils")
	    }
	
	    # ifultools
	    ifultools.pos <- attached.where("ifultools", nomatch=0)
	    if (!ifultools.pos) {
	      if (!quietRequire("ifultools")) {
	        cat("Installing required library: ifultools.\n")
	        install.packages("ifultools", lib=.libPaths()[1], destdir=tmpdir)
	      }
	
	      cat("Loading required library: ifultools.\n")
	      library("ifultools")
	    }
	
	    # wmtsa
	    wmtsa.pos <- attached.where("wmtsa", nomatch=0)
	    if (!wmtsa.pos) {
	      if (!quietRequire("wmtsa")) {
	        cat("Installing required library: wmtsa.\n")
	        install.packages("wmtsa", lib=.libPaths()[1], destdir=tmpdir)
	      }
	
	      cat("Loading required library: wmtsa.\n")
	      library("wmtsa")
	    }
	
	    # DBI
	    DBI.pos <- attached.where("DBI", nomatch=0)
	    if (!DBI.pos) {
	      if (!quietRequire("DBI")) {
	        cat("Installing required library: DBI.\n")
	        install.packages("DBI", lib=.libPaths()[1], destdir=tmpdir)
	      }
	
	      cat("Loading required library: DBI.\n")
	      library("DBI")
	    }
	
	    # RSQLite
	    RSQLite.pos <- attached.where("RSQLite", nomatch=0)
	    if (!RSQLite.pos) {
	      if (!quietRequire("RSQLite")) {
	        cat("Installing required library: RSQLite.\n")
	        install.packages("RSQLite", lib=.libPaths()[1], destdir=tmpdir)
	      }
	
	      cat("Loading required library: RSQLite.\n")
	      library("RSQLite")
	    }
		}

    invisible(NULL)
  }

	.Last.lib <- function(library, section, .data, where)
	{
	  if (is.ui.app("s+gui") && is.element("SPlusMenuBar$msProcess", guiGetObjectNames("MenuItem")))
	    guiRemove("MenuItem", Name="SPlusMenuBar$msProcess")  
	 
	  invisible(NULL)
	}

} # end is.R() check
