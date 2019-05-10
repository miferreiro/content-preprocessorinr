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
        MeasureLengthPipe$new()$pipe("length_before_cleaning_text") %>|%
        StripHTMLPipe$new()$pipe()  %>|%
        FindUserNamePipe$new()$pipe() %>|%
        FindHashtagPipe$new()$pipe() %>|%
        FindUrlPipe$new()$pipe() %>|%
        FindEmoticonPipe$new()$pipe() %>|%
        FindEmojiPipe$new()$pipe() %>|%
        GuessLanguagePipe$new()$pipe() %>|%
        ContractionsPipe$new()$pipe() %>|%
        AbbreviationPipe$new()$pipe() %>|%
        SlangPipe$new()$pipe() %>|%
        ToLowerCasePipe$new()$pipe() %>|%
        InterjectionPipe$new()$pipe() %>|%
        StopWordPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_cleaning_text") %>|%
        TeeCSVPipe$new()$pipe() #%>|%
        # StringBuffer2SynsetVectorPipe$new()$pipe() %>|%
        # SynsetVector2SynsetFeatureVectorPipe$new()$pipe() %>|%
        # TeeCSVFromSynsetFeatureVectorPipe$new()$pipe()
      
      return(instance)
    }
  )
)