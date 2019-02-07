#Class to 
#
#
#Variables:
#
#
FindUserNameInStringBufferPipe <- R6Class(
    
  "FindUserNameInStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "@userName",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[FindUserNameInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindUserNameInStringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindUserNameInStringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    }, 
    
    userPattern = "(?:\\s|^|[\"><¡¿?!;:,.'-])(@[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",
        
    pipe = function(instance, removeUser = TRUE){

      if (!"Instance" %in% class(instance)) {
        stop("[FindUserNameInStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeUser)) {
          stop("[FindUserNameInStringBufferPipe][pipe][Error]
                  Checking the type of the variable: removeUser ", 
                    class(removeUser))
      }
              
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "FindUserNameInStringBufferPipe")
      
      if (!super$checkCompatibility("FindUserNameInStringBufferPipe")) {
        stop("[FindUserNameInStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
             
      instance$getData() %>>% 
        self$findUserName() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}
      
      if (removeUser) {
        instance$getData()  %>>%
          self$replaceUserName() %>>%
            instance$setData()
      }
      
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe UserName")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
      
      return(instance)
    },
    
    replaceUserName = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[FindUserNameInStringBufferPipe][replaceUserName][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_replace_all(data,
                             regex(self$userPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    },
    
    findUserName = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[FindUserNameInStringBufferPipe][findUserName][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_match_all(data,
                           regex(self$userPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    }
  )
)