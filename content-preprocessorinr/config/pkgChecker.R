
packages.list <- c("R6","stringi",
                   "rlist","XML",
                   "tools","devtools",
                   "twitteR","ROAuth",
                   "bitops","RCurl",
                   "rjson","ndjson",
                   "streamR","rtweet",
                   "rlang","tuber",
                   "rvest","pipeR",
                   "magrittr","data.table",
                   "readr","rJava","plyr",
                   "rvest","textclean",
                   "pipeR","purrr",
                   "stringr","httr",
                   "ini","devtools",
                   "urltools", "cld2",
                   "jwatjars","jwatr",
                   "pacman", "tibble",
                   "parallelMap","textclean",
                   "utf8", "textutils",
                   "httpuv","rex",
                   "R.lang","tokenizers"
                   )

if (!require('devtools')) {
    install.packages("devtools")
    library("devtools")
}

if (!require('jwatr')) {
  if (!require('urltools')) {
    install.packages('urltools')
  }
  library('urltools')
  devtools::install_github("hrbrmstr/jwatr")
  
  library(jwatr)
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh(
    "trinker/lexicon",    
    "trinker/textclean"
)

if (!require("R.lang")) {
  devtools::install_github("HenrikBengtsson/R.lang")
}

cat("[PkgChecker][INFO] Package Manager\n")

checkPackages <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) { 
    cat("[PkgChecker][checkPackages][INFO]",length(new.packages)," packages needed to execute aplication\n Installing packages ...\n")
    suppressMessages(install.packages(new.packages,repos ="https://ftp.cixug.es/CRAN/", verbose = FALSE))
  }
}

loadPackages <- function(packages) {
  unload.packages <- packages[!(packages %in% (.packages() ) )  ]
  if (length(unload.packages) > 0) { 
    cat("[PkgChecker][loadPackages][INFO] ",length(unload.packages)," required packages not loaded\n Loading packages ...\n", sep="")
    for (lib in unload.packages ) {
      cat("[PkgChecker][loadPackages][INFO] ",lib,"\n", sep="")
      library(lib, verbose = FALSE, quietly = FALSE, character.only = TRUE) 
    }
  } else cat("[PkgChecker][loadPackages][INFO] All packages are loaded\n")
}

verifyandLoadPackages <- function() {
  checkPackages(packages.list)
  loadPackages(packages.list)
}

verifyandLoadPackages()

rm(packages.list)
rm(checkPackages)
rm(loadPackages)
rm(verifyandLoadPackages)
