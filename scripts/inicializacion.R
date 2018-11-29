{
    setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
    source("scripts/pkgChecker.R")
    rm(checkPackages)
    rm(loadPackages)
    rm(verifyandLoadPackages)
{
    source("scripts/conexiones.R")
    source("scripts/dataSource.R")
    source("scripts/dataSms.R")
    source("scripts/dataTwtid.R")
    source("scripts/dataWarc.R")
    source("scripts/dataEml.R")
    source("scripts/dataTytb.R")
    source("scripts/dataYtbid.R")
    source("scripts/pipesFunction.R")
    source("scripts/funcionesGenerales.R")
    
    #EML
    source("scriptsLibrary/eml/eml.R")
    
    #WARC
    source("scriptsLibrary/warc-master/R/aaa.r")
    source("scriptsLibrary/warc-master/R/as_warc.r")
    source("scriptsLibrary/warc-master/R/cdx.r")
    source("scriptsLibrary/warc-master/R/create_cdx.r")
    source("scriptsLibrary/warc-master/R/create_warc.r")
    source("scriptsLibrary/warc-master/R/process_entry.r")
    source("scriptsLibrary/warc-master/R/process_info.r")
    source("scriptsLibrary/warc-master/R/process_request.r")
    source("scriptsLibrary/warc-master/R/process_response.r")
    source("scriptsLibrary/warc-master/R/RcppExports.R")
    source("scriptsLibrary/warc-master/R/read_warc_entry.r")
    source("scriptsLibrary/warc-master/R/utils.r")
    source("scriptsLibrary/warc-master/R/validate.r")
    source("scriptsLibrary/warc-master/R/warc-package.R")
    source("scriptsLibrary/warc-master/R/write_warc_record.r")
    

}
    
    arcAll <- list.files(path = archivosTest,
                         # pattern = "twtid|tsms|eml|tytb|ytbid",
                         #pattern="tsms|twtid|eml|tytb|ytbid",
                        pattern ="www"
                         ,recursive = TRUE
                         ,full.names = TRUE
                         ,all.files = TRUE)
    #arcAll <- "content-preprocessor/tests/spamassassin/_ham_/00003.860e3c3cee1b42ead714c5c874fe25f7.eml"
    #example of multipart/signed eml
    #arcAll <- "content-preprocessor/tests/spamassassin/_ham_/00003.19be8acd739ad589cd00d8425bac7115.eml"
    
    listaInstancias <- sapply(arcAll, DataSource$public_methods$createInstance)
    listaInstanciasValidas <- list()
    invalid = list();

    fun <- FuncionesGenerales$new()
    funcionesPipes <- pipesFunctions$new()

    propiedadesTextoDate = function(x){
        
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

        print(x$getPath())
    }
    
    deleteInvalidInstances = function(x){
       
        if(x$getSource() == "" || x$getSource() == "error" || x$getDate() == "" || x$getDate() == "error" ) {
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
        return (listaInstanciasValidas);
    }
    
    propiedadesIniciales = function(x){
        #x$addProperties(fun$getTarget(x$getPath()),"target")
        x$addProperties(fun$getExtension(x$getPath()),"extension")
        x$addProperties(fun$getDateCreate(x$getPath()),"dateCreate")
        #x$addProperties(fun$getEncode(x$getPath()),"encode")
        x$addProperties(fun$getEncode2(x$getPath()),"encode2")
        x$addProperties(fun$getEncodeConfidence(x$getPath()),"encodeConfidence")
        x$addProperties(fun$getEncodeLanguage(x$getPath()),"encodeLanguage")
        x$addProperties(fun$getLanguage(x$getSource()),"language")
        x$addProperties(fun$getLanguageScore(x$getSource()),"languageScore")
        x$addProperties(fun$getLanguagePercent(x$getSource()),"languagePercent")
        x$addProperties(fun$getLength(x$getSource()),"length")
        x$setData(x$getSource())
    }
    
    #ver paquete promises
    pipes = function(x){
        # x$getSpecificProperties('data') %>>% funcionesPipes$toLowerSource() %>>% ~data
        x$getData() %>>%
           funcionesPipes$deleteEspaciosMultiples() %>>%
           # funcionesPipes$deleteDireccionesEmail() %>>%
          
            {x$setData(.)}
        # x$setSpecificProperties('data', data)
    }
    
    
}
