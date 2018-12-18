{setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")

arcCache <- list.files(path = "content-preprocessorinr/testFiles/cache",
                   recursive = TRUE
                     ,full.names = TRUE
                     ,all.files = TRUE)
invisible(file.remove(arcCache))
}

