#Class to obtain the length of the data
#
# 
#
#Variables:
#
StringBuffer2SynsetVectorPipe <- R6Class(
  
  "StringBuffer2SynsetVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "", 
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #
      #Args:
      #   propertyName: (character) 
      #
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
    # An array of UnmatchedTextHandlers to fix incorrect text fragments
    # vUTH = list(UrbanDictionaryHandler$new(), 
    #             TyposHandler$new(), 
    #             ObfuscationHandler$new()),
    vUTH = list(UrbanDictionaryHandler$new()),
    # List of puntuation marks accepted on the beggining of a word
    acceptedCharOnBeggining = "¿¡[(\"\\'",
    acceptedCharOnBegginingPattern = "^[¿¡\\[\\(\"\\'][¿¡\\[\\(\"\\']*",
    # List of puntuation marks accepted on the end of a word
    acceptedCharOnEnd = ".,!?)];:\"\\'",
    acceptedCharOnEndPattern = "[\\.,!?\\)\\];:<>\"\\'][\\.,!?\\)\\];:<>\"\\']*$",
    # List of puntuation marks accepted on the middle of a word
    acceptedCharOnMiddle = "/-.,;:",
    acceptedCharOnMiddlePattern = "[\\/\\()\\)\\-\\.,;:<>][\\/\\-\\.,;:<>]*",
    # A pattern to detect puntuation marks
    puntMarkPattern = "[[:punct:]]",
    
    
    # This method find fagments in text (str) thar are incorrect.
    # 
    # @param str The original text
    # @param lang The language of the original text
    # @return A vector of pairs (T,R) where T is the incorrect fragment and R
    # will be the replacement (null now)  
    computeUnmatched = function(str, lang) {
      
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

          # We found a puntuation mark in the token
          # matcher.start() <- here is the index of the puntuation mark
          # We developed rules checking also the existence of term/terms in Babelnet
          
          # if do not fit the rules and/or not found in Babelnet
              # returnValue.add(new Pair<String,String>(current,null));
          # To check the exitence of the term in BabelNet, we will 
          # create a class org.ski4spam.util.BabelNetUtils with  
          # static methods.
          
          indexOfPuntMark <- regexpr(self$puntMarkPattern, current)[1]
          
          if (indexOfPuntMark == 1) { #The puntuation symbol is at the beggining
            if (indexOf(self$acceptedCharOnBeggining, substr(current, indexOfPuntMark, indexOfPuntMark)) == -1) {
              returnValue <- list.append(returnValue, NULL)
              names(returnValue)[length(returnValue)] <- current
            } else {
              
              match <- gregexpr(self$acceptedCharOnBegginingPattern, current, perl = T)[[1]][1]
              length <- attr(match, "match.length")
              if (is.null(length)) {
                print("Length is null")
                length <- 1
              }
              if (!babelUtils$isTermInBabelNet(substr(current, match + length, nchar(current)), lang)) {
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
              }
            }
          } else {
            if (indexOfPuntMark == nchar(current)) { #the puntuation symbol is at the end
              if (indexOf(self$acceptedCharOnEnd, substr(current, indexOfPuntMark, indexOfPuntMark)) == -1) {
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
              } else {
                if (!babelUtils$isTermInBabelNet(substr(current, 1, indexOfPuntMark - 1), lang)) {
                  returnValue <- list.append(returnValue, NULL)
                  names(returnValue)[length(returnValue)] <- current
                }
              }
              
            } else {#The puntuation symbol is in the middle
                if (indexOf(self$acceptedCharOnMiddle,substr(current, indexOfPuntMark, indexOfPuntMark)) == -1 &&
                      indexOf(self$acceptedCharOnEnd,substr(current, indexOfPuntMark, indexOfPuntMark)) == -1) {
                
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
                
              } else {
                
                match <- gregexpr(self$acceptedCharOnEndPattern, current, perl = T)[[1]]
                length <- attr(match, "match.length")
                
                if (match == indexOfPuntMark) {
                  if (!babelUtils$isTermInBabelNet(substr(current, 1, indexOfPuntMark - 1), lang)) {
                    returnValue <- list.append(returnValue, NULL)
                    names(returnValue)[length(returnValue)] <- current
                  }
                } else {
                  
                  match <- gregexpr(self$acceptedCharOnMiddlePattern, current)[[1]]
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
          # if current is not found in Babelnet
          #     returnValue.add(new Pair<String,String>(current,null));
          if (!babelUtils$isTermInBabelNet(current, lang)) {
            returnValue <- list.append(returnValue, NULL)
            names(returnValue)[length(returnValue)] <- current
          }
        }
      }
      
      return(returnValue)
        
        
    },
    # Try to fix terms that are incorrectly written (and are not found in
    # Wordnet) The original text should be fixed according with the replacements made
    # 
    # @param originalText The originalText to fix
    # @param unmatched A list of text fragments that should be tryed to fix.
    # The text fragments are in the form of a pair (T,R) where T is the
    # original fragment ant R the replacement (null originally). This method
    # should fill R with the suggested replacement
    # @return A string containing the original text fixed
    handleUnmatched = function(originalText, unmatched, lang) {
      # Implement the UnmatchedTextHandler interface and three specific 
      # implementations that are:
      #   + UrbanDictionaryHandler
      #   + TyposHandler
      #   + ObfuscationHandler
      
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
    # 
    # Create a synsetVector from text
    # 
    # @param fixedText The text to transform into a synset vector
    # @param lang The language in which the original text is written
    # @return A vector of synsets. Each synset is represented in a pair (S,T)
    # where S stands for the synset ID and T for the text that matches this
    # synset ID
    # 
    buildSynsetVector = function(fixedText, lang) {
      
      # Call Babelfy api to transform the string into a vector of sysnsets. 
      # The fisrt string in the pair is the synsetID from babelnet
      # The second string is the matched text
      # The dictionary (dict) should be updated by adding each detected synset in texts.
      # 
      # //Query Babelnet
      # ArrayList<Pair<String, String>> returnValue = 
      #   BabelUtils.getDefault().buildSynsetVector(fixedText, lang);
      # 
      # //Update dictionaries
      # for (Pair<String, String> current : returnValue) {
      #   SynsetDictionary.getDictionary().add(current.getObj1());
      # }
      
      returnValue <- list()
      
      
      returnValue <- babelUtils$buildSynsetVector(fixedText, lang)

      for (i in 1:length(returnValue)) {
        synsetDictionary$add(names(returnValue)[[i]])
      }
            
      return(returnValue)
    },  
    # 
    # Compute synsets from text. This method get data from StringBuffer and
    # process instances:
    # <li>Invalidate instance if the language is not present</li>
    # <li>Get the list of unmatched texts</li>
    # <li>Process this texts to get matches</li>
    # <li>Build a synset vector</li>
    # 
    pipe = function(instance,
                    propertyName = super$getPropertyName(),
                    nchar_conf = TRUE) {
      #
      #
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
      
      if (!"character" %in% class(propertyName)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"logical" %in% class(nchar_conf)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: nchar_conf ", 
                  class(nchar_conf))
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
      
      instance$addProperties(sv, "synsetVector")
      
      return(instance);
    }
  ),
  
  private = list(
    propertyLanguageName = ""
  )
)
