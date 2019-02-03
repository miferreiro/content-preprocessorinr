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
        File2StringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe() %|%
        StripHTMLFromStringBufferPipe$new()$pipe()  %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_html_drop") %|%
        FindUserNameInStringBufferPipe$new()$pipe() %|%
        FindHashtagInStringBufferPipe$new()$pipe() %|%
        FindUrlInStringBufferPipe$new()$pipe() %|%
        FindEmoticonInStringBufferPipe$new()$pipe() %|%
        FindEmojiInStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_cleaning_text") %|%
        GuessLanguageFromStringBufferPipe$new()$pipe(languageTwitter = FALSE) %|%
        AbbreviationFromStringBufferPipe$new()$pipe(removeAbbreviations = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_abbreviation") %|%
        StringBufferToLowerCasePipe$new()$pipe() %|%
        SlangFromStringBufferPipe$new()$pipe(removeSlangs = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_slang") %|%
        InterjectionFromStringBufferPipe$new()$pipe(removeInterjections = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_interjection") %|%
        StopWordFromStringBufferPipe$new()$pipe(removeStopWords = TRUE) %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_stopwords") %|%
        TeeCSVFromStringBufferPipe$new()$pipe(withData = T)
      
        #NERFromStringBufferPipe$new()$pipe() %|% #Sin implementar
        #StringBuffer2SynsetVectorPipe$new()$pipe() %|% #Sin implementar
        #new SynsetVector2SynsetFeatureVectorPipe(SynsetVectorGroupingStrategy.COUNT), #Sin implementar

      
      return(instance)
    }
  )
)