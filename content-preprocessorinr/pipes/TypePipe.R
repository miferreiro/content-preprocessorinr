#Class to establish the flow of pipes
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
      #Function where the flow of the pipes is created
      #
      #Args: 
      #   instance: (Instance) The instance that is going to be processed
      #
      #Returns: 
      #   The preprocessed instance
      #             
      if (!"Instance" %in% class(instance)) {
        stop("[TypePipe][pipeAll][Error] 
                Checking the type of the variable: instance ", 
                  class(instance));
      }
      
      instance %>|%
        TargetAssigningPipe$new()$pipe() %>|%
        StoreFileExtensionPipe$new()$pipe() %>|%
        GuessDatePipe$new()$pipe() %>|%
        File2Pipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe() %>|%
        StripHTMLPipe$new()$pipe()  %>|%
        MeasureLengthPipe$new()$pipe("length_after_html_drop") %>|%
        FindUserNamePipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_user") %>|%
        FindHashtagPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_hashtag") %>|%
        FindUrlPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_url") %>|%
        FindEmoticonPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_emoticon") %>|%
        FindEmojiPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_emoji") %>|%
        GuessLanguagePipe$new()$pipe() %>|%
        ContractionsPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_contractions") %>|%
        AbbreviationPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_abbreviation") %>|%
        SlangPipe$new()$pipe() %>|%
        ToLowerCasePipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_slang") %>|%
        InterjectionPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_interjection") %>|%
        StopWordPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_stopwords") %>|%
        TeeCSVPipe$new()$pipe() 
        
      return(instance)
    }
  )
)