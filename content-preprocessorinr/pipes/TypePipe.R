#Class to establish the flow of pipes
#
#Variables:
#flowPipes: (list) list of the pipes that have been executed so far
#banPipes: (list) pipes that can not be executed after

TypePipe <- R6Class(
  
  "TypePipe",
  
  public = list(
    
    initialize = function() {
      
    },
    
    pipeAll = function(instance) {
      #
      #Function where the flow of the pipes is created
      #Function where the flow of the pipes is created
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
        TargetAssigningFromPathPipe$new()$pipe() %>|%
        StoreFileExtensionPipe$new()$pipe() %>|%
        GuessDateFromFilePipe$new()$pipe() %>|%
        File2StringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe() %>|%
        StripHTMLFromStringBufferPipe$new()$pipe()  %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_html_drop") %>|%
        FindUserNameInStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_user") %>|%
        FindHashtagInStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_hashtag") %>|%
        FindUrlInStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_url") %>|%
        FindEmoticonInStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_emoticon") %>|%
        FindEmojiInStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_emoji") %>|%
        GuessLanguageFromStringBufferPipe$new()$pipe() %>|%
        ContractionsFromStringBuffer$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_contractions") %>|%
        AbbreviationFromStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_abbreviation") %>|%
        SlangFromStringBufferPipe$new()$pipe() %>|%
        StringBufferToLowerCasePipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_slang") %>|%
        InterjectionFromStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_interjection") %>|%
        StopWordFromStringBufferPipe$new()$pipe() %>|%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_stopwords") %>|%
        TeeCSVFromStringBufferPipe$new()$pipe() 
        
      TypePipe[["private_fields"]][["flowPipes"]] <- list()
      TypePipe[["private_fields"]][["banPipes"]] <- list()
      
      
      return(instance)
    }
  ),
  
  private = list(
    flowPipes = list() ,
    banPipes = list()
  )
)