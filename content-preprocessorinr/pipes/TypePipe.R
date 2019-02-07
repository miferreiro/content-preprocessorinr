#Class to 
#
#
#Variables:
#

TypePipe <- R6Class(
  
  "TypePipe",
  
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
        stop("[TypePipe][pipeAll][Error] 
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
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_user") %|%
        FindHashtagInStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_hashtag") %|%
        FindUrlInStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_url") %|%
        FindEmoticonInStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_emoticon") %|%
        FindEmojiInStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_emoji") %|%
        GuessLanguageFromStringBufferPipe$new()$pipe() %|%
        ContractionsFromStringBuffer$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_contractions") %|%
        AbbreviationFromStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_abbreviation") %|%
        StringBufferToLowerCasePipe$new()$pipe() %|%
        SlangFromStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_slang") %|%
        InterjectionFromStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_interjection") %|%
        StopWordFromStringBufferPipe$new()$pipe() %|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_stopwords") %|%
        TeeCSVFromStringBufferPipe$new()$pipe()
      
      return(instance)
    }
  )
)