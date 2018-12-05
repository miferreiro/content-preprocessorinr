{
    setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
    Sys.setlocale("LC_TIME","UK")
    #Sys.setlocale("LC_TIME","Spanish")
    source("content-preprocessorinr/config/pkgChecker.R")
    rm(checkPackages)
    rm(loadPackages)
    rm(verifyandLoadPackages)
{
    source("content-preprocessorinr/config/conexiones.R")
    source("content-preprocessorinr/extractor/dataSource.R")
    source("content-preprocessorinr/extractor/dataSms.R")
    source("content-preprocessorinr/extractor/dataTwtid.R")
    source("content-preprocessorinr/extractor/dataWarc.R")
    source("content-preprocessorinr/extractor/dataEml.R")
    source("content-preprocessorinr/extractor/dataTytb.R")
    source("content-preprocessorinr/extractor/dataYtbid.R")
    source("content-preprocessorinr/functions/pipesFunction.R")
    source("content-preprocessorinr/functions/funcionesGenerales.R")
    
    #EML
    source("content-preprocessorinr/scripts/libraries/eml/eml.R")
    
    #WARC
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/aaa.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/as_warc.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/cdx.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/create_cdx.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/create_warc.r")
    source("content-preprocessorinr/scripts/libraries/warc-master/R/process_entry.r")
    source("content-preprocessorinr/scripts/libraries/warc-master/R/process_info.r")
    source("content-preprocessorinr/scripts/libraries/warc-master/R/process_request.r")
    source("content-preprocessorinr/scripts/libraries/warc-master/R/process_response.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/RcppExports.R")
    source("content-preprocessorinr/scripts/libraries/warc-master/R/read_warc_entry.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/utils.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/validate.r")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/warc-package.R")
    # source("content-preprocessorinr/scripts/libraries/warc-master/R/write_warc_record.r")
    
}
    arcAll <- list.files(path = archivosTest,
                         pattern=patternLista
                         ,recursive = TRUE
                         ,full.names = TRUE
                         ,all.files = TRUE)

    listaInstancias <- sapply(arcAll, DataSource$public_methods$createInstance)
    listaInstanciasValidas <- list()
    invalid = list();

    fun <- FuncionesGenerales$new()
    funcionesPipes <- pipesFunctions$new()

    propiedadesTextoDate = function(x){
        print(x$getPath())
        x$addProperties(fun$getTarget(x$getPath()),"target")
        
        tryCatch(x$obtainSource(),
                 warning = function(w) {
                 print(paste("Source main warning(", w ,") in ", x$getPath()));
                },
                error = function(e) {
                    print(paste("Source main error(", e ,") in ", x$getPath()));
                })
        tryCatch(x$obtainDate(),
                 warning = function(w) {
                     print(paste("Date main warning(", w ,") in ", x$getPath()));
                 },
                 error = function(e) {
                     print(paste("Date main error(", e ,") in ", x$getPath()));
                 })

        ifelse(!(validUTF8(x$getSource())),
          {  
            mensaje <- c( "el archivo " , x$getPath() , " no es utf8")
            warning(mensaje)
          }
        ,"")
    }
    
    deleteInvalidInstances = function(x){
       
        if(x$getSource() == "" || x$getSource() == "error" || x$getSpecificProperties("target") == "unrecognizable") {
               return(FALSE)
        }else{
            return(TRUE)
        }
    }
    
    obtainValidInstances = function(){
        cont = 1;
        for(elem in listaInstancias){
            if(invalid[[cont]]){
                listaInstanciasValidas <- list.append(listaInstanciasValidas,elem);
                names(listaInstanciasValidas)[length(listaInstanciasValidas)] <- names(listaInstancias)[cont];
            }
            cont = cont + 1;
        }
        rm(cont)
        return(listaInstanciasValidas);
    }
    
    propiedadesIniciales = function(x){
        x$addProperties(fun$getExtension(x$getPath()),"extension")
        x$addProperties(fun$getDateCreate(x$getPath()),"dateCreate")
        x$addProperties(fun$getLength(x$getSource()),"length")
        x$addProperties(fun$getEncode(x$getSource()),"encode")
        x$addProperties(fun$getEncode2(x$getPath()),"encode2")
        x$addProperties(fun$getEncodeConfidence(x$getPath()),"encodeConfidence")
        x$addProperties(fun$getEncodeLanguage(x$getPath()),"encodeLanguage")
        x$addProperties(fun$getLanguage(x$getSource()),"language")
        x$addProperties(fun$getLanguageScore(x$getSource()),"languageScore")
        x$addProperties(fun$getLanguagePercent(x$getSource()),"languagePercent")
    
        x$setData(x$getSource())
    }
    
    #ver paquete promises
    pipes = function(x){ 
        x$getData() %>>% 
            funcionesPipes$StringBufferToLowerCasePipe() %>>%
            funcionesPipes$StripHTMLFromStringBufferPipe() %>>%
            funcionesPipes$deleteEspaciosMultiples() %>>%
            funcionesPipes$StopWordFromStringBuffer() %>>%
            funcionesPipes$FindUrlInStringBufferPipe() %>>%
            funcionesPipes$FindUserNameInStringBufferPipe() %>>%
            funcionesPipes$FindHashtagInStringBufferPipe() %>>%
            {x$setData(.)}
        # x$getSpecificProperties('data') %>>% funcionesPipes$toLowerSource() %>>% ~data
        # x$setSpecificProperties('data', data)
    }
}
