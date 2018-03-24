#' @name kobo_projectinit
#' @rdname kobo_projectinit
#' @title  Analysis project initiation
#'
#' @description    Create analysis project structure
#'
#' @return A structure of directory and scripts in order to set up quickly a project.
#'
#' @export kobo_projectinit
#'
#' @author Edouard Legoupil, Elliott MEesseiller
#'
#' @examples
#' kobo_projectinit()
#'


kobo_projectinit <- function() {
  mainDir <- getwd()

  subDir <- "perso"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Perso exists in mainDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("perso directory exists in your project directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("perso directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile=paste0(mainDir,"/perso/README.md")
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is where your user name, password and config are stored"), fileConn)
    close(fileConn)
  }


  subDir <- "code"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Code exists in mainDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Code directory exists in your project directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("Code directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile=paste0(mainDir,"/code/README.md")
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is where analysis scripts are saved"), fileConn)
    close(fileConn)
  }

  ## Need to test if we have multiple .libPaths()

  path <- as.data.frame(.libPaths())
  if (nrow(path)==1) {path_correct <- as.character(path[1,1])
  } else{ cat("You have multiple library path! \n")
     if( dir.exists(file.path(path[1,1],"/KoboAnalyser")) ){
       path_correct <- as.character(path[1,1])
     } else{ path_correct <- as.character(path[2,1])}
    }

  destfile=paste0(mainDir,"/code/0-packages.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script/0-packages.R",sep=""), destfile)
  }

  destfile=paste0(mainDir,"/code/0-theme.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//0-theme.R",sep=""), destfile)
  }

  destfile=paste0(mainDir,"/code/0-config.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//0-config.R",sep=""), destfile)
  }

  destfile=paste0(mainDir,"/code/1-loaddata.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//1-loaddata.R",sep=""), destfile)
  }

  destfile=paste0(mainDir,"/code/2-create-graph.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//2-create-graph.R",sep=""), destfile)
  }

  destfile=paste0(mainDir,"/code/3-create-indicators.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//3-create-indicators.R",sep=""), destfile)
  }

  destfile=paste0(mainDir,"/code/4-generate-report.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//4-generate-report.R",sep=""), destfile)
  }
  destfile=paste0(mainDir,"/code/report.Rmd")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//report.Rmd",sep=""), destfile)
  }
  destfile=paste0(mainDir,"/code/3-create-indicators.R")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/script//3-create-indicators.R",sep=""), destfile)
  }

  subsubDir <- "shiny_app"
  if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/", sep = "/", collapse = "/"))) {
    cat("shiny_app exists in subDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, subsubDir, sep = "/", collapse = "/"))) {
    cat("shiny_app directory exists in your project directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("shiny_app directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir,subsubDir))
  }

  destfile=paste0(mainDir,"/code/shiny_app/app_koboanalyser.R")
    file.copy(paste(path_correct,"/KoboAnalyser/shiny_examples//app_koboanalyser.R",sep=""), destfile, overwrite = TRUE)


  subsubsubDir <- "www"
  if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/",subsubsubDir, "/",sep = "/", collapse = "/"))) {
    cat("www exists in subDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, subsubDir,subsubsubDir, sep = "/", collapse = "/"))) {
    cat("www directory exists in your project directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("www directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir,subsubDir,subsubsubDir))
  }
  destfile=paste0(mainDir,"/code/shiny_app/www/exportformat.png")
  if (!file.exists(destfile)) {
    file.copy(paste(path_correct,"/KoboAnalyser/shiny_app/www//exportformat.png",sep=""), destfile)
  }


  subDir <- "data"

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Data exists in mainDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Data directory exists in your project directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("Data directory does not exist in your project directory - creating now! \n")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile=paste0(mainDir,"/data/README.md")
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is the one where are stored data in CSV format, the form in XLS format and geodata in SHP format",
                 "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!",
                 "",
                 "This project is only to keep track of your analysis workflow"), fileConn)
    close(fileConn)
  }


  subDir <- "out"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Out directory exists in your project directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Ouput exists in your project directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("Out directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile=paste0(mainDir,"/out/README.md")
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is where the analysis output will be generated"))
    close(fileConn)
  }

  source("code/0-packages.R") # Make sure that all the packages necessary are loaded.


}
NULL
