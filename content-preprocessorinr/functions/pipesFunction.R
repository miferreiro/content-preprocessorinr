{
    pipesFunctions <- R6Class(
        
        "FuncionesPipes",
        
        public = list(
            URLPattern = '((?:\\s|^)(?:(?:[a-z0-9]+:)(?:\\/\\/|\\/|)?|(www.))(?:[\\w-]+(?:(?:\\.[\\w-]+)+))(?:[\\w.,@?^=%&:\\/~+#-]*[\\w@?^=%&\\/~+#-])?(?=(?:,|;|!|:|\"|\\?|\\s|$)))',
          # emailPattern = "(?:\\s|^|¡)([\\w!#$%&'*+-\\/=?^_`\\{|\\}~"(),:;<>@\\[\\]\"ç]+@[\\[\\w.-:]+([A-Z]{2,4}|\\]))[;:\\?\"!,.]?(?=(?:\\s|$))",
          userPattern = "((?:\\s|^|[\"¿¡])(@[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:\\?\"!,.]?(?=(?:\\s|$)))",
          hashtagPattern = "(?:\\s|^|[\"¿¡])(#[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.]?(?=(?:\\s|$))",
          
            StringBufferToLowerCasePipe = function(texto){
                return(tolower(texto));
            },
            
            FindUrlInStringBufferPipe = function(texto){
                return(str_replace_all(texto,regex(self$URLPattern ,ignore_case = TRUE, multiline=TRUE), "-------------------------"))
            },
          
            FindUserNameInStringBufferPipe =function(texto){
                return(str_replace_all(texto,regex(self$userPattern,ignore_case =TRUE,multiline = TRUE), ""))
            },
          
            StopWordFromStringBuffer = function(texto){
                return(str_replace_all(texto,"[[:punct:]]", " "))
            },

            deleteEspaciosMultiples = function(texto){
                return(str_replace_all(texto,"[\\s]+", " "))
            },

            StripHTMLFromStringBufferPipe = function(texto){
                return(replace_html(texto))
            },
            FindHashtagInStringBufferPipe = function(texto){
                return(str_replace_all(texto,regex(self$hashtagPattern,ignore_case = TRUE,multiline = TRUE), " "))
            },
          
          
          #ver paquete promises
          pipes = function(x){ 
              x$getData() %>>% 
                  self$StringBufferToLowerCasePipe() %>>%
                  self$StripHTMLFromStringBufferPipe() %>>%
                  self$deleteEspaciosMultiples() %>>%
                  self$StopWordFromStringBuffer() %>>%
                  self$FindUrlInStringBufferPipe() %>>%
                  self$FindUserNameInStringBufferPipe() %>>%
                  self$FindHashtagInStringBufferPipe() %>>%
                  {x$setData(.)}
              # x$getSpecificProperties('data') %>>% funcionesPipes$toLowerSource() %>>% ~data
              # x$setSpecificProperties('data', data)
          }

        )
    )
}