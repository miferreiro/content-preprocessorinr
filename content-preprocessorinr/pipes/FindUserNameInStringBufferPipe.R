#Class to find and/or replace the users on the data
#
#Variables:
#
#userPattern: (character) Regular expression to detect users
# 
FindUserNameInStringBufferPipe <- R6Class(
    
  "FindUserNameInStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "userName",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #           
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
      #
      #Function that preprocesses the instance to obtain/replace the users
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   removeUser: (logical) indicate if the users are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #     
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
              
      TypePipe[["private_fields"]][["flowPipes"]] <- 
        list.append(TypePipe[["private_fields"]][["flowPipes"]], "FindUserNameInStringBufferPipe")
      
      if (!super$checkCompatibility("FindUserNameInStringBufferPipe")) {
        stop("[FindUserNameInStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
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
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe UserName")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindUserNameInStringBufferPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findUserName = function(data) {
      #
      #Function that find the users in the data
      #
      #Args:
      #   data: (character) instance to preproccess
      #Returns:
      #   list with users found
      #        
      if (!"character" %in% class(data)) {
        stop("[FindUserNameInStringBufferPipe][findUserName][Error] 
                Checking the type of the variable: data ", 
             class(data))
      }
      
      return(str_match_all(data,
                           regex(self$userPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },
    
    replaceUserName = function(data) {
      #
      #Function that remove the users in the data 
      #
      #Args:
      #   data: (character) instance to preproccess
      #Returns:
      #   data with users removed
      #          
      if (!"character" %in% class(data)) {
        stop("[FindUserNameInStringBufferPipe][replaceUserName][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_replace_all(data,
                             regex(self$userPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)