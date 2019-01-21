#Class to 
#
#
#Variables:
#

SerialPipes <- R6Class(
  
  "SerialPipes",
  
  public = list(
    
    initialize = function() {
      
    },
    
    pipeAll = function(instance) {
    
      #
      #
      #Args: 
      #   
      #
      #Returns: 
      #   
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[SerialPipes][pipeAll][Error] 
             Checking the type of the variable: instance ", 
             class(instance));
      }
      
      instance %>>% 
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
        #FindEmojiInStringBufferPipe$new()$pipe() %>>%#Falta la expresion regular
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_cleaning_text") %>>%#Hecho
        GuessLanguageFromStringBufferPipe$new()$pipe(languageTwitter = FALSE) %>>%#Hecho, completar con lo del idioma del twitter.No detecta euskera, por lo que los archivos en resource no sirven
        AbbreviationFromStringBufferPipe$new()$pipe() %>>% #Falta contRolar las abreviaciones con caracteres especiales
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_abbreviation") %>>%#Hecho
        StringBufferToLowerCasePipe$new()$pipe() %>>%#Hechoc
        SlangFromStringBufferPipe$new()$pipe() %>>% #Falta contRolar las abreviaciones con caracteres especiales
        #StringBuffer2SynsetVectorPipe$new()$pipe() %>>% #Sin implementar
        InterjectionFromStringBufferPipe$new()$pipe() %>>% #Falta contRolar las interjeciones con caracteres especiales
        StopWordFromStringBufferPipe$new()$pipe() %>>% #Falta contRolar las stop words con caracteres especiales
        #NERFromStringBufferPipe$new()$pipe() %>>% #Sin implementar
        # #TeeCSVFromStringBufferPipe$new()$pipe() %>>% #Sin implementar # new TeeCSVFromStringBufferPipe("output.csv", true), Esperar a quitar las stopWords 
        # StringBuffer2SynsetVectorPipe$new()$pipe() %>>% #Sin implementar
        # new SynsetVector2SynsetFeatureVectorPipe(SynsetVectorGroupingStrategy.COUNT), #Sin implementar
        # TeeCSVFromStringBufferPipe$new()$pipe() %>>% #Sin implementar # new TeeCSVFromSynsetFeatureVectorPipe("outputsyns.csv"), 
        {instance}
      
      return(instance)
    }
  )
)