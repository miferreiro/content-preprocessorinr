{
rm(list = ls()) 
t <- proc.time()
#setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")
#Carga todos los archivos .R
source("content-preprocessorinr/config/sourceLoad.R")
#Inicializamos el objeto que manejará los diferentes tipos de conexiones: youtube y twitter
connections <- Connections$new();
Files <- list.files(path = "content-preprocessorinr/testFiles/tests"
                    ,recursive = TRUE
                    ,full.names = TRUE
                    ,all.files = TRUE)


#Creamos la lista de instancias, las cuales contendran el date, source, path,data y una lista de propiedades
#del archivo que se encuentra en el path indicado
InstancesList <- sapply(Files, Builder$new()$createInstance)

#Crear una clase abstracta que sirve como template para los diferentes pipes
#Ademas que tenga un método para que se ejecuten todos los pipes que se indiquen
pipes <- function(x){
    x %>>% 
        TargetAssigningFromPathPipe$new()$pipe() %>>%#Hecho
        StoreFileExtensionPipe$new()$pipe() %>>%#Hecho
        GuessDateFromFilePipe$new()$pipe() %>>%#Hecho
        File2StringBufferPipe$new()$pipe() %>>%#Hecho (Refactorizar el código para hacerlo más optimizado y que se entienda mejor)
        MeasureLengthFromStringBufferPipe$new()$pipe() %>>%#Hecho
        StripHTMLFromStringBufferPipe$new()$pipe() %>>% #Utiliza replace_html del paquete textclean, ver otras alternativas
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_html_drop") %>>%#Hecho
        FindUserNameInStringBufferPipe$new()$pipe() %>>%#Hecho
        FindHashtagInStringBufferPipe$new()$pipe() %>>%#Hecho
        FindUrlInStringBufferPipe$new()$pipe() %>>%#Hecho
        FindEmoticonInStringBufferPipe$new()$pipe() %>>%#Hecho
        FindEmojiInStringBufferPipe$new()$pipe() %>>%#Falta la expresion regular
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_cleaning_text") %>>%#Hecho
        GuessLanguageFromStringBufferPipe$new()$pipe(languageTwitter = FALSE) %>>%#Hecho, completar con lo del idioma del twitter
        AbbreviationFromStringBufferPipe$new()$pipe() %>>% #Sin implementar
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_abbreviation") %>>%#Hecho
        StringBufferToLowerCasePipe$new()$pipe() %>>%#Hecho
        StringBuffer2SynsetVectorPipe$new()$pipe() %>>% #Sin implementar
        InterjectionFromStringBufferPipe$new()$pipe() %>>% #Sin implementar
        StopWordFromStringBufferPipe$new()$pipe() %>>% #Sin implementar
        NERFromStringBufferPipe$new()$pipe() %>>% #Sin implementar
        # #TeeCSVFromStringBufferPipe$new()$pipe() %>>% #Sin implementar # new TeeCSVFromStringBufferPipe("output.csv", true), Esperar a quitar las stopWords 
        StringBuffer2SynsetVectorPipe$new()$pipe() %>>% #Sin implementar
        # new SynsetVector2SynsetFeatureVectorPipe(SynsetVectorGroupingStrategy.COUNT), #Sin implementar
        # TeeCSVFromStringBufferPipe$new()$pipe() %>>% #Sin implementar # new TeeCSVFromSynsetFeatureVectorPipe("outputsyns.csv"), 
        {x}
}

#Se aplica a todas las instancias los pipes definidos en la funcion pipes
InstancesList <- sapply(InstancesList, pipes)

ValidInstancesList <- list();
invalidBooleanList <- list();

#Obtenemos la lista de instanciasInvalidas
invalidBooleanList <- lapply(InstancesList,deleteInvalidInstances)
#A partir de la lista instancias invalidas y las lista de instancias originales, obtenemos la lista de instancias validas
ValidInstancesList <- obtainValidInstances(InstancesList,invalidBooleanList)


cat("Time: ",proc.time() - t )
}

#
#parallelStart(mode = "multicore")
# parallelStart(mode = "local")
# parallelStartSocket(2)
# parallelSource(files = "content-preprocessorinr/config/sourceLoad.R")
# parallelExport(objnames = "generalFun");
# parallelExport(objnames = "connections");
# parallelExport(objnames = "builder");
# InstancesList <- sapply(Files, builder$createInstance)



# library(parallel)
# 
# numCores <- detectCores()
# cl <- makeCluster( numCores - 1)
# 
#  library(foreach)
#  sourceList <- list("content-preprocessorinr/processFiles/processTwtid.R",
#                     "content-preprocessorinr/processFiles/processYtbid.R",
#                     "content-preprocessorinr/processFiles/processWarc.R",
#                     "content-preprocessorinr/processFiles/processEml.R",
#                     "content-preprocessorinr/processFiles/processSmsTytb.R"
#                   )
#
#  invisible( foreach(x = sourceList
#                     ,.combine = 'list'
#                     ,.multicombine = TRUE
#                     ,.verbose =  FALSE
#                     ,.inorder = FALSE) %dopar%
#  {
#              filesTestPath = "content-preprocessorinr/testFiles/tests";
#              source("content-preprocessorinr/inicializacion.R");
#              source(x);
#              rm(x)
#              rm(filesTestPath)
# })
# 
# InstancesList <- unlist(list.append(EmlInstancesList
#                                     ,TwtidInstancesList
#                                     ,YtbidInstancesList
#                                     ,WarcInstancesList
#                                     ,SmsTytbInstancesList))
# 
