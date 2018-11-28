{
    pipesFunctions <- R6Class(
        
        "FuncionesPipes",
        
        public = list(
            
            StringBufferToLowerCasePipe = function(texto){
                return(tolower(texto));
            },
            
            FindUrlInStringBufferPipe =function(texto){
                return(str_replace_all(texto,"http\\S*", ""))
            },
            FindUserNameInStringBufferPipe =function(texto){
                return(str_replace_all(texto,"(?:\\s|^|[\"¿¡])(@[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:\\?\"!,.]?(?=(?:\\s|$))", ""))
            },           
            StopWordFromStringBuffer = function(texto){
                return(str_replace_all(texto,"[[:punct:]]", " "))
            },
            
            deleteEspaciosMultiples = function(texto){
                return(str_replace_all(texto,"[\\s]+", " "))
            },
            
            deleteDireccionesEmail = function(texto){
                return(str_replace_all(texto,"[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*"))
            },
            
            StripHTMLFromStringBufferPipe = function(texto){
                return(replace_html(texto))
            },
            FindHashtagInStringBufferPipe = function(texto){
                return(str_replace_all(texto,"(?:\\s|^|[\"¿¡])(#[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.]?(?=(?:\\s|$))", " "))
            }
            
        )
    )
}