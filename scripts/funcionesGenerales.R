{
    FuncionesGenerales <- R6Class(
        
        "FuncionesGenerales",
        
        public = list(
            
            getLanguage = function(data, ...) {
                return(detectLanguage(data,...)[[1]])
            },
            getLanguageScore = function(data,...){
                return(detectLanguage(data,...)[[7]])  
            },
            getLanguagePercent = function(data,...){
                return(detectLanguage(data,...)[[10]])  
            },           
            getLength = function(data, nchar_conf = TRUE, ...) {
                return(ifelse(nchar_conf,nchar(data,...),object.size(data)))
            },
            getEncode = function(path,...){
                return(Encoding(data))
            },
            getEncode2 = function(path,...){
                return(stri_enc_detect(path)[[1]][[1]][1])
            },
            getEncodeConfidence = function(path, ...){
                return(stri_enc_detect(path)[[1]][[3]][1])
            },
            getEncodeLanguage = function(path, ...){
                return(stri_enc_detect(path)[[1]][[2]][1])
            },
            getExtension = function(path, ...){
                return(file_ext(path))
            },
            getDateCreate = function(path, ...){
                return(as.character.Date(file.info(path)[["ctime"]],...))
            },
            getTarget = function(path,...) {
                if (grepl("_ham_", path)) {
                    aux <- "ham"
                } else{
                    if (grepl("_spam_", path)) {
                        aux <- "spam"
                    } else{
                        aux <- "unrecognizable"
                    }
                }
                return(aux)
            },
            toCsv = function(dataFrame){
                propiedadesNames <- c('path','source','date','data','target','extension','dateCreate'
                                      ,'encode2','encodeConfidence','encodeLanguage'
                                      ,'language','languageScore','languagePercent','length')
                
                
                a <- data.frame(matrix(unlist(
                    sapply(dataFrame,function(x){
                        c(x$getPath(),x$getSource(),x$getDate(),x$getData(),x$getProperties())
                    }))
                    , nrow = length(dataFrame),ncol = length(propiedadesNames),byrow=TRUE),stringsAsFactors = FALSE)
                
                names(a) <- propiedadesNames
                write.csv2(a,file = "propiedades.csv");
            }
           
            )
        )
}

