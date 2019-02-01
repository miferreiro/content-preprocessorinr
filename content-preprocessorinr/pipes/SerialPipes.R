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
      
      instance %|%
        TargetAssigningFromPathPipe$new()$pipe() %|%
        StoreFileExtensionPipe$new()$pipe() %|%
        GuessDateFromFilePipe$new()$pipe() %|%
        File2StringBufferPipe$new()$pipe()  %|% #Hecho (Refactorizar el c?digo para hacerlo m?s optimizado y que se entienda mejor) Mejorar eml y solucionar problema con un tweet, asegurarse que los warcs se leen en utf-8
        MeasureLengthFromStringBufferPipe$new()$pipe() %|%
        StripHTMLFromStringBufferPipe$new()$pipe()  %|% #Esperar otras alternativas
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_html_drop") %|%
        FindUserNameInStringBufferPipe$new()$pipe() %|%  #No se invalida? Determinar condicion de cuando se invalida
        FindHashtagInStringBufferPipe$new()$pipe() %|% #No se invalida? Determinar condicion de cuando se invalida
        FindUrlInStringBufferPipe$new()$pipe() %|% #No se invalida? Determinar condicion de cuando se invalida
        FindEmoticonInStringBufferPipe$new()$pipe() %|% #No se invalida? Determinar condicion de cuando se invalida
        FindEmojiInStringBufferPipe$new()$pipe() %|% #Falta la expresion regular y No se invalida? Determinar condicion de cuando se invalida
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_cleaning_text") %|%
        GuessLanguageFromStringBufferPipe$new()$pipe(languageTwitter = FALSE) %|%
        AbbreviationFromStringBufferPipe$new()$pipe(removeAbbreviations = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_abbreviation") %|%
        StringBufferToLowerCasePipe$new()$pipe() %|%
        SlangFromStringBufferPipe$new()$pipe(removeSlangs = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_slang") %|%
        # StringBuffer2SynsetVectorPipe$new()$pipe() %|% #Sin implementar
        InterjectionFromStringBufferPipe$new()$pipe(removeInterjections = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_interjection") %|%
        StopWordFromStringBufferPipe$new()$pipe(removeStopWords = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_stopwords") %|%
        TeeCSVFromStringBufferPipe$new()$pipe(withData = T)
       
        #NERFromStringBufferPipe$new()$pipe() %|% #Sin implementar
        #TeeCSVFromStringBufferPipe$new()$pipe() %|% #Sin implementar # new TeeCSVFromStringBufferPipe("output.csv", true), Esperar a quitar las stopWords 
        #StringBuffer2SynsetVectorPipe$new()$pipe() %|% #Sin implementar
        #new SynsetVector2SynsetFeatureVectorPipe(SynsetVectorGroupingStrategy.COUNT), #Sin implementar
        #TeeCSVFromStringBufferPipe$new()$pipe() %|% #Sin implementar # new TeeCSVFromSynsetFeatureVectorPipe("outputsyns.csv"), 
        
      
      return(instance)
    }
  )
)