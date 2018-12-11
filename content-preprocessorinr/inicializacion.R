{
    setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
    Sys.setlocale("LC_TIME","UK")
    #Sys.setlocale("LC_TIME","Spanish")
    source("content-preprocessorinr/config/pkgChecker.R")

{
    source("content-preprocessorinr/config/connections.R")
    source("content-preprocessorinr/extractor/extractorSource.R")
    source("content-preprocessorinr/extractor/extractorSms.R")
    source("content-preprocessorinr/extractor/extractorTwtid.R")
    source("content-preprocessorinr/extractor/extractorWarc.R")
    source("content-preprocessorinr/extractor/extractorEml.R")
    source("content-preprocessorinr/extractor/extractorTytb.R")
    source("content-preprocessorinr/extractor/extractorYtbid.R")
    source("content-preprocessorinr/functions/pipesFunction.R")
    source("content-preprocessorinr/functions/GeneralFunctions.R")
    
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
    generalFun <- GeneralFunctions$new()
    pipesFun <- pipesFunctions$new()

    propertiesSourceDate = function(x){
        print(x$getPath())
        x$addProperties(generalFun$getTarget(x$getPath()),"target")
        
        tryCatch(x$obtainSourceDate(),
                 warning = function(w) {
                    cat("SourceDate main warning(", paste(w) ,") in ",x$getPath());
                    cat("\n");
                },
                error = function(e) {
                    cat("SourceDate main error(", paste(e) ,") in ",x$getPath());
                    cat("\n");
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
    
    obtainValidInstances = function(InstancesList,invalidBooleanList){
        cont = 1;
        for(elem in InstancesList){
            if(invalidBooleanList[[cont]]){
                ValidInstancesList <- list.append(ValidInstancesList,elem);
                names(ValidInstancesList)[length(ValidInstancesList)] <- names(InstancesList)[cont];
            }
            cont = cont + 1;
        }
        rm(cont)
        return(ValidInstancesList);
    }
    
    initialProperties = function(x){
        x$addProperties(generalFun$getExtension(x$getPath()),"extension")
        x$addProperties(generalFun$getDateCreate(x$getPath()),"dateCreate")
        x$addProperties(generalFun$getLength(x$getSource()),"length")
        x$addProperties(generalFun$getEncode(x$getSource()),"encode")
        x$addProperties(generalFun$getEncode2(x$getPath()),"encode2")
        x$addProperties(generalFun$getEncodeConfidence(x$getPath()),"encodeConfidence")
        x$addProperties(generalFun$getEncodeLanguage(x$getPath()),"encodeLanguage")
        x$addProperties(generalFun$getLanguage(x$getSource()),"language")
        x$addProperties(generalFun$getLanguageScore(x$getSource()),"languageScore")
        x$addProperties(generalFun$getLanguagePercent(x$getSource()),"languagePercent")
    
        x$setData(x$getSource())
    }
    
    #ver paquete promises
    pipes = function(x){ 
        x$getData() %>>% 
            pipesFun$StringBufferToLowerCasePipe() %>>%
            pipesFun$StripHTMLFromStringBufferPipe() %>>%
            pipesFun$deleteEspaciosMultiples() %>>%
            pipesFun$StopWordFromStringBuffer() %>>%
            pipesFun$FindUrlInStringBufferPipe() %>>%
            pipesFun$FindUserNameInStringBufferPipe() %>>%
            pipesFun$FindHashtagInStringBufferPipe() %>>%
            {x$setData(.)}
        # x$getSpecificProperties('data') %>>% funcionesPipes$toLowerSource() %>>% ~data
        # x$setSpecificProperties('data', data)
    }
    return();
}
