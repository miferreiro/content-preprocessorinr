#packages.list <- c("R6","caret","tictoc","mltools","here","parallel","RWeka","devtools","ggrepel")
packages.list <- c("R6","stringi",
                   "cldr","rlist",
                   "tools","devtools",
                   "twitteR","ROAuth",
                   "bitops","RCurl",
                   "rjson","ndjson",
                   "streamR","rtweet",
                   "rlang","tuber",
                   "rvest","pipeR",
                   "magrittr","data.table","readr","rJava","plyr"
                   )
if(!require('cldr')){
    url <- "http://cran.us.r-project.org/src/contrib/Archive/cldr/cldr_1.1.0.tar.gz"
    pkgFile<-"cldr_1.1.0.tar.gz"
    download.file(url = url, destfile = pkgFile)
    install.packages(pkgs=pkgFile, type="source", repos=NULL)
    unlink(pkgFile)
    rm(url)
    rm(pkgFile)
}
if(!require('warc')){
    if(!require('Rcpp')){
        install.packages("Rcpp")
    }
    library(Rcpp)
    devtools::install_git("https://gitlab.com/hrbrmstr/warc.git")
    library(warc)
}
# if(!require('rPython')){
#     url <- "https://github.com/cjgb/rPython-win.git"
#     pkgFile<-"rPython.tar.gz"
#     download.file(url = url, destfile = pkgFile)
#     install.packages(pkgs=pkgFile, type="source", repos=NULL)
#     unlink(pkgFile)
#     rm(url)
#     rm(pkgFile)
# }
#Descomentar para obtener date y source eml
#packages.list <- c("R6","stringi","textcat","rlist","tools","devtools","REmail")

cat("[PkgChecker][INFO] Package Manager\n")

checkPackages <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){ 
    cat("[PkgChecker][INFO]",length(new.packages)," packages needed to execute aplication\n Installing packages ...\n")
    suppressMessages(install.packages(new.packages,repos="https://ftp.cixug.es/CRAN/", verbose = FALSE))
  }
}

loadPackages <- function(packages){
  unload.packages <- packages[!(packages %in% (.packages() ) )  ]
  if(length(unload.packages) > 0){ 
    cat("[PkgChecker][INFO] ",length(unload.packages)," required packages not loaded\n Loading packages ...\n", sep="")
    for( lib in unload.packages ){
      cat("[PkgChecker][INFO] ",lib,"\n", sep="")
      library(lib, verbose = FALSE, quietly = FALSE, character.only = TRUE) 
    }
  }else cat("[PkgChecker][INFO] All packages are loaded\n")
}

verifyandLoadPackages <- function(){
  checkPackages(packages.list)
  loadPackages(packages.list)
}

verifyandLoadPackages()
rm(packages.list)

