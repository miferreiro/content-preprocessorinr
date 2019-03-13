#A pipe to compute synsets from text
#
#Variables:
#propertyLanguageName: (character) the name of property about language
#vUTH: (list) An array of UnmatchedTextHandlers to fix incorrect text fragments
#acceptedCharOnBeggining: (character) List of puntuation marks accepted on the beggining of a word
#acceptedCharOnBegginingPattern: (character) List of puntuation marks accepted on the beggining of a word
#acceptedCharOnEnd: (character) List of puntuation marks accepted on the end of a word
#acceptedCharOnEndPattern: (character) List of puntuation marks accepted on the end of a word
#acceptedCharOnMiddle: (character) List of puntuation marks accepted on the middle of a word
#acceptedCharOnMiddlePattern: (character) List of puntuation marks accepted on the middle of a word
#puntMarkPattern: (character) A pattern to detect puntuation marks
# 
StringBuffer2SynsetVectorPipe <- R6Class(
  
  "StringBuffer2SynsetVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "synsetVector", 
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated.
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   propertyLanguageName: (character) Name of the language property
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #                
      if (!"character" %in% class(propertyName)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
             Checking the type of the variable: propertyName ", 
             class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
             class(propertyLanguageName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      
    },
  
    getPropertyLanguageName = function() {
      #
      #Getter of name of property language
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of propertyLanguageName variable
      #
      return(private$propertyLanguageName)
    },
    # vUTH = list(UrbanDictionaryHandler$new(), 
    #             TyposHandler$new(), 
    #             ObfuscationHandler$new()),
    vUTH = list(UrbanDictionaryHandler$new()),
    acceptedCharOnBeggining = "¿¡[(\"'",
    acceptedCharOnBegginingPattern = "^[¿¡\\[\\(\"'][¿¡\\[\\(\"']*",
    acceptedCharOnEnd = ".,!?)];:\"'",
    acceptedCharOnEndPattern = "[.,!?\\)\\];:<>\"'][.,!?\\)\\];:<>\"']*$",
    acceptedCharOnMiddle = "/-.,;:",
    acceptedCharOnMiddlePattern = "[-\\/\\()\\).,;:<>][-\\/.,;:<>]*",
    puntMarkPattern = "[[:punct:]]",
    
    computeUnmatched = function(str, lang) {
      #
      #This method find fagments in text (str) thar are incorrect.
      #
      #Args:
      #   str: (character) The original text
      #   lang: (character) The language of the original text
      #Returns:
      # A list where the name is the incorrect fragment and the value
      # will be the replacement (null now)  
      #      
      if (!"character" %in% class(str)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: str ", 
                  class(str))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      # The value that will be returned
      returnValue <- list()
      
      st <- tokenize_regex(x = str, pattern = "( |\\t|\\n|\\r|\\u000b|\\f)+", simplify = TRUE)

      for (current in st) {
        
        if (grepl(self$puntMarkPattern, current)) {
          
          indexOfPuntMark <- regexpr(self$puntMarkPattern, current)
          
          if (indexOfPuntMark == 1) { #The puntuation symbol is at the beggining
            if (indexOf(self$acceptedCharOnBeggining, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1) {
              returnValue <- list.append(returnValue, NULL)
              names(returnValue)[length(returnValue)] <- current
            } else {
              
              match <- regexpr(self$acceptedCharOnBegginingPattern, current)
              length <- attr(match, "match.length")

              if (!babelUtils$isTermInBabelNet(substr(current, match + length, nchar(current)), lang)) {
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
              }
            }
          } else {
            if (indexOfPuntMark == nchar(current)) { #the puntuation symbol is at the end
              if (indexOf(self$acceptedCharOnEnd, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1) {
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
              } else {
                if (!babelUtils$isTermInBabelNet(substr(current, 1, indexOfPuntMark - 1), lang)) {
                  returnValue <- list.append(returnValue, NULL)
                  names(returnValue)[length(returnValue)] <- current
                }
              }
              
            } else {#The puntuation symbol is in the middle
                if (indexOf(self$acceptedCharOnMiddle, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1 &&
                      indexOf(self$acceptedCharOnEnd, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1) {
                
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
                
              } else {
                
                match <- regexpr(self$acceptedCharOnEndPattern, current)
                length <- attr(match, "match.length")
                
                if (match == indexOfPuntMark) {
                  if (!babelUtils$isTermInBabelNet(substr(current, 1, indexOfPuntMark - 1), lang)) {
                    returnValue <- list.append(returnValue, NULL)
                    names(returnValue)[length(returnValue)] <- current
                  }
                } else {
                  
                  match <- regexpr(self$acceptedCharOnMiddlePattern, current)
                  length <- attr(match, "match.length")
                  
                  if (match != -1) {
                    
                    firstElement <- substr(current, 1, match[1] - 1)
                    lastElement <- substr(current, match[1] + length[1], nchar(current))
                    
                    if (!babelUtils$isTermInBabelNet(firstElement, lang) || 
                        (match[1] + length[1] < nchar(current) && !babelUtils$isTermInBabelNet(lastElement, lang))) {
                      returnValue <- list.append(returnValue, NULL)
                      names(returnValue)[length(returnValue)] <- current
                    }
                  } else {
                    returnValue <- list.append(returnValue, NULL)
                    names(returnValue)[length(returnValue)] <- current
                  }
                }
              }
            }
          }
        } else {
          # We check if the term current exist in babelnet. 
          if (!babelUtils$isTermInBabelNet(current, lang)) {
            returnValue <- list.append(returnValue, NULL)
            names(returnValue)[length(returnValue)] <- current
          }
        }
      }
      
      return(returnValue)
        
    },

    handleUnmatched = function(originalText, unmatched, lang) {
      #
      # Try to fix terms that are incorrectly written (and are not found in
      # Wordnet) The original text should be fixed according with the replacements made
      #
      # Implement the UnmatchedTextHandler interface and three specific 
      # implementations that are:
      #   + UrbanDictionaryHandler
      #   + TyposHandler
      #   + ObfuscationHandler
      # 
      #Args:
      #   originalText: (character) The originalText to fix
      #   unmatched: (list) A list of text fragments that should be tryed to fix.
      #                     The text fragments are in the form of a pair (T,R) 
      #                     where T is the original fragment ant R the replacement 
      #                     (null originally). This method should fill R with the 
      #                     suggested replacement
      #   lang: (character) The language of the original text
      #Returns:
      #     A string containing the original text fixed 
      #      
      
      if (!"character" %in% class(originalText)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }
      
      if (!"list" %in% class(unmatched)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: unmatched ", 
                  class(unmatched))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      returnValue <- originalText
      
      for (i in 1:length(unmatched)) {
        if (!length(self$vUTH) == 0) {
          for (x in 1:length(self$vUTH)) {
            
            current <- self$vUTH[[x]]$handle(names(unmatched)[[i]], unmatched[[i]], lang)
            
            if (!is.null(current)) {
              break
            }
          }
        }
        
        if (!is.null(unmatched[[i]])) {
          returnValue <- gsub(pattern = names(unmatched)[[i]], 
                              replacement = unmatched[[i]], 
                              x = returnValue, 
                              fixed = T)
        }
      }
      
      return(returnValue)
    },

    buildSynsetVector = function(fixedText, lang) {
      #
      #Create a synsetVector from text
      #
      #Call Babelfy api to transform the string into a vector of sysnsets. 
      #The fisrt string in the pair is the synsetID from babelnet.
      #The second string is the matched text.
      # 
      #Args:
      #   fixedText: (character) The text to transform into a synset vector
      #   lang: (character) The language in which the original text is written
      #Returns:
      #    A list of synsets. Each synset is represented in a pair (S,T)
      #    where S stands for the synset ID and T for the text that matches this
      #    synset ID
      #         
      if (!"character" %in% class(fixedText)) {
        stop("[StringBuffer2SynsetVectorPipe][buildSynsetVector][Error] 
                Checking the type of the variable: fixedText ", 
                  class(fixedText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[StringBuffer2SynsetVectorPipe][buildSynsetVector][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      returnValue <- list()
      
      returnValue <- babelUtils$buildSynsetVector(fixedText, lang)

      for (i in 1:length(returnValue)) {
        synsetDictionary$add(names(returnValue)[[i]])
      }
            
      return(returnValue)
    },  

    pipe = function(instance) {
      #Compute synsets from text. This method get data from StringBuffer and
      #process instances:
      # - Invalidate instance if the language is not present
      # - Get the list of unmatched texts
      # - Process this texts to get matches
      # - Build a synset vector
      # 
      #Args:
      #   instance: (Instance) instance to preprocces
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #          
      
      if (!"Instance" %in% class(instance)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
    
      cat("[StringBuffer2SynsetVectorPipe][pipe][Info]", "Data:", instance$getData(), "\n")
      sv <- SynsetVector$new(instance$getData())
      
      # Invalidate the instance if the language is not present
      # We cannot correctly represent the instance if the language is not present
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not language property")
        instance$invalidate()
        warning(message)  
        
        return(instance)
      }

      sv$setUnmatchedTexts(self$computeUnmatched(sv$getOriginalText(), 
                           toupper(languageInstance)))
      
      if (length(sv$getUnmatchedTexts()) > 0) {
        sv$setFixedText(self$handleUnmatched(sv$getOriginalText(), 
                                             sv$getUnmatchedTexts(),
                                             toupper(languageInstance)))
      } else {
        sv$setFixedText(sv$getOriginalText())
      }
      
      sv$setSynsets(self$buildSynsetVector(sv$getFixedText(),
                                           toupper(languageInstance)))
      
      instance$addProperties(sv, super$getPropertyName())
      
      return(instance);
    }
  ),
  
  private = list(
    propertyLanguageName = ""
  )
)
