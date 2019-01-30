#Class to build Instance types
#
#
#Variables:
#

FactoryMethod <- R6Class(
    
  "FactoryMethod",
    
  public = list(
        
    initialize = function() {
            
    },
        
    createInstance = function(path) {
      #
      #Function that builds instances from the path it receives
      #
      #Depending on the extension of the file indicated in the path, 
      #an instance type or other is created
      #
      #Args: 
      #   path: (character) 
      #
      #Returns: 
      #   The corresponding object according to the file extension
      #           
      if (!"character" %in% class(path)) {
        stop("[FactoryMethod][createInstance][Error] 
                Checking the type of the variable: path ", 
                  class(path))
      }
        
      switch(file_ext(path),
       `eml` =  return(ExtractorEml$new(path)),
       `tsms` = return(ExtractorSms$new(path)),
       `twtid` = return(ExtractorTwtid$new(path)),
       `ttwt` = return(ExtractorTtwt$new(path)),
       `warc` = return(ExtractorWarc$new(path)),
       `tytb` = return(ExtractorTytb$new(path)),
       `ytbid` = return(ExtractorYtbid$new(path))
      )
      
      return()
    }
  )
)