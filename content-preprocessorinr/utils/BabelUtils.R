#This class encapsulates all required information to support Babelfy and
#Babelnet queryies
#
#Variables:
#
#stopSynset: (character) A stop synset to navigate in Babelnet hierarchy. 
#                        The synset means entity.
#MAX_BABELFY_QUERY: (numeric) String limit for babelfy queries
#key: (character) has the key of babelfy/babelnet
#baseBabelNet: (character) url base to access the api rest
#baseBabelfy: (character) url base to access the api rest
BabelUtils <- R6Class(
  
  "BabelUtils",
  
  public = list(
    
    initialize = function(pathKeys) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of key. This variable
      #contains the key that are stored in the file indicated in the variable
      #
      #Args:
      #   pathKeys: (character) Path of the .ini file that contains the keys
      #
      #Returns:
      #   null
      # 
      if (!"character" %in% class(pathKeys)) {
        stop("[BabelUtils][initialize][Error]
                Checking the type of the variable: pathKeys ",
                  class(pathKeys))
      }
    
      if (!"ini" %in% file_ext(pathKeys)) {
        stop("[BabelUtils][initialize][Error]
                Checking the extension of the file: pathKeys ",
                  file_ext(pathKeys))
      }
    
      private$key <- read.ini(pathKeys)$babelfy$key
    },
    
    baseBabelNet = "https://babelnet.io/v5/",
    baseBabelfy = "https://babelfy.io/v1/",

    isTermInBabelNet = function(term, lang) {
      #
      #Determines whether a term is included in Babelnet or not
      #
      #Args:
      #   term: (character) The term to check
      #   lang: (character) The language in which the term is written
      #
      #Returns:
      #   true if the term is included in Babelnet ontological dictionary
      #      
      if (!"character" %in% class(term)) {
        stop("[BabelUtils][isTermInBabelNet][Error]
                Checking the type of the variable: term ",
                  class(term))
      }      
      
      if (!"character" %in% class(lang)) {
        stop("[BabelUtils][isTermInBabelNet][Error]
                Checking the type of the variable: lang ",
                  class(lang))
      }
      
      if (toupper(trim(lang)) %in% "UND") {
        cat("[BabelUtils][isTermInBabelNet][Error]",
            "Unable to query Babelnet because language is not found.","\n")
        return(FALSE)
      }
      
      endpoint <- "getSynsetIds"
      call <- paste(self$baseBabelNet,
                    endpoint,
                    "?",
                    "lemma",
                    "=",
                    term,
                    "&",
                    "searchLang",
                    "=",
                    lang,
                    "&",
                    "key",
                    "=",
                    private$key,
                    sep = "")
      
      get_information_word <- GET(call)
      get_information_word <- content(get_information_word,"text")
      get_information_word <- jsonlite::fromJSON(get_information_word, flatten = TRUE)
      
      while ("message" %in% names(get_information_word)) {
        print(get_information_word)

        cat("[BabelUtils][isTermInBabelNet][Info]", get_information_word$message,"\n")
        
        now <- Sys.time() 
        
        midnight <- as.POSIXct(paste(as.character(Sys.Date() + 1), 
                                     "01:01:01", 
                                     sep = " "), 
                               format = "%Y-%m-%d %H:%M:%S")
        
        diffHour <- unclass(midnight - now)[1]
        
        cat("[BabelUtils][isTermInBabelNet][Info]", "Now ", paste(as.POSIXct(now)), "\n")
        cat("[BabelUtils][isTermInBabelNet][Info]", "Midnight ", paste(as.POSIXct(midnight)), "\n")
        cat("[BabelUtils][isTermInBabelNet][Info]", "Waiting... ", diffHour, " hours\n")
        
        Sys.sleep(diffHour * 60 * 60)
        
        get_information_word <- GET(call)
        get_information_word <- content(get_information_word, "text")
        get_information_word <- jsonlite::fromJSON(get_information_word, flatten = TRUE)
      } 
      
      cat("[BabelUtils][isTermInBabelNet][Info]","El termino: ", term, " :", 
          class(get_information_word) %in% "data.frame", "\n")
      
      return(class(get_information_word) %in% "data.frame")
    },

    checkSynsetInBabelnet = function(synsetToCheck, textToLink) {
      #
      #Determines whether a term is included in Babelnet or not
      #
      #Args:
      #   synsetToCheck: (character) The Synset to check
      #   textToLink: (character) The word which corresponds with the Synset in Babelfy.
      #                           This word is provided only to create a log report.
      #
      #Returns:
      #   true if is possible to obtain information about synset in Babelnet,
      #   so the synset is included in Babelnet ontological dictionary.
      #   
      if (!"character" %in% class(synsetToCheck)) {
        stop("[BabelUtils][checkSynsetInBabelnet][Error]
                Checking the type of the variable: synsetToCheck ",
                  class(synsetToCheck))
      }
      
      if (!"character" %in% class(textToLink)) {
        stop("[BabelUtils][checkSynsetInBabelnet][Error]
                Checking the type of the variable: textToLink ",
                  class(textToLink))
      }
      
      endpoint <- "getSynset"
      call <- paste(self$baseBabelNet,
                    endpoint,
                    "?",
                    "id",
                    "=",
                    synsetToCheck,
                    "&",
                    "key",
                    "=",
                    private$key,
                    sep = "")
      
      get_information_synset <- GET(call)
      get_information_synset <- content(get_information_synset, "text")
      get_information_synset <- jsonlite::fromJSON(get_information_synset, flatten = TRUE)
      
      while("message" %in% names(get_information_synset)) {
        print(get_information_synset)
        cat("[BabelUtils][checkSynsetInBabelnet][Info]", get_information_synset$message,"\n")
        
        now <- Sys.time() 
        
        midnight <- as.POSIXct(paste(as.character(Sys.Date() + 1), 
                                     "01:01:01", 
                                     sep = " "), 
                               format = "%Y-%m-%d %H:%M:%S")
        
        diffHour <- unclass(midnight - now)[1]
        
        cat("[BabelUtils][checkSynsetInBabelnet][Info]", "Now ", paste(as.POSIXct(now)), "\n")
        cat("[BabelUtils][checkSynsetInBabelnet][Info]", "Midnight ", paste(as.POSIXct(midnight)), "\n")
        cat("[BabelUtils][checkSynsetInBabelnet][Info]", "Waiting... ", diffHour, " hours\n")
        
        Sys.sleep(diffHour * 60 * 60)
        get_information_synset <- GET(call)
        get_information_synset <- content(get_information_synset, "text")
        get_information_synset <- jsonlite::fromJSON(get_information_synset, flatten = TRUE)
      }
      
      if (length(get_information_synset$senses) > 0) {
        cat("[BabelUtils][checkSynsetInBabelnet][Info]", "The text [", textToLink,
            "] obtained in Babelfy as [", synsetToCheck, "] exists in Babelnet. ",
            "\n")
        
        return(TRUE)
        
      } else {
        
        cat("[BabelUtils][checkSynsetInBabelnet][Error]", "The text [", textToLink, 
            "] obtained in Babelfy as [", synsetToCheck, "] does not exists in Babelnet. ",
            "\n")
        
        return(FALSE)
      }
    },

    buildSynsetVector = function(fixedText, lang) {
      #
      #Build a list of sysntets from a text
      #
      #Args:
      #   fixedText: (character) The text to be transformed into synsets
      #   lang: (character) The language to identify the synsets
      #
      #Returns:
      #   A vector of synsets. 
      #     
      if (!"character" %in% class(fixedText)) {
        stop("[BabelUtils][buildSynsetVector][Error]
                Checking the type of the variable: fixedText ",
                  class(fixedText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[BabelUtils][buildSynsetVector][Error]
                Checking the type of the variable: lang ",
                  class(lang))
      }
      
      remain <- fixedText
      parts <- list()
      
      #The text is separated into parts and saved in parts 
      while (nchar(remain) > self$getMaxBabelfyQuery()) {
        
        segment <- substr(remain, 0, self$getMaxBabelfyQuery)
        
        splitPos <- R.lang::lastIndexOf(segment, ".")
        
        if (splitPos == -1) {
          splitPos <- R.lang::lastIndexOf(segment, " ")
        }
        
        if (splitPos == -1) {
          splitPos <- self$getMaxBabelfyQuery()
        }

        parts <- list.append(parts, substr(remain, 0, splitPos))
        remain <- substr(remain, splitPos)

      }
      
      parts <- list.append(parts, remain)
      # print("Length of the parts")
      # print(length(parts))
      # View(parts)
      # Make the requests of each part and save the queries in bfyAnnotations
      bfyAnnotations <- list()

      currentPart <- parts[[1]]
      endpoint <- "disambiguate"
      currentPart <- gsub("[[:space:]]", "+", currentPart)
      call <- paste(self$baseBabelfy,
                    endpoint,
                    "?",
                    "text",
                    "=",
                    currentPart,
                    "&",
                    "searchLang",
                    "=",
                    lang,
                    "&",
                    "key",
                    "=",
                    private$key,
                    sep = "")
      
      get_disambiguate_text <- GET(call)
      get_disambiguate_text <- content(get_disambiguate_text, "text")
      get_disambiguate_text <- jsonlite::fromJSON(get_disambiguate_text, flatten = TRUE)
      
      while ("message" %in% names(get_disambiguate_text)) {
        print(get_disambiguate_text)
        cat("[BabelUtils][buildSynsetVector][Info]", get_disambiguate_text$message, "\n")
        
        now <- Sys.time() 
        
        midnight <- as.POSIXct(paste(as.character(Sys.Date() + 1), 
                                     "01:01:01", 
                                     sep = " "), 
                              format = "%Y-%m-%d %H:%M:%S")
        
        diffHour <- unclass(midnight - now)[1]
        
        cat("[BabelUtils][buildSynsetVector][Info]", "Now ", paste(as.POSIXct(now)), "\n")
        cat("[BabelUtils][buildSynsetVector][Info]", "Midnight ", paste(as.POSIXct(midnight)), "\n")
        cat("[BabelUtils][buildSynsetVector][Info]", "Waiting... ", diffHour, " hours\n")
        
        Sys.sleep(diffHour * 60 * 60)
        get_disambiguate_text <- GET(call)
        get_disambiguate_text <- content(get_disambiguate_text, "text")
        get_disambiguate_text <- jsonlite::fromJSON(get_disambiguate_text, flatten = TRUE)
      }
      # print("get_disambiguate_text")
      # print(get_disambiguate_text)
      bfyAnnotations <- list.append(bfyAnnotations, get_disambiguate_text)
      
      #We put together the data.frame of each query stored in the bfyAnnotations list
      dataFrameAnnotation <- data.frame()
      
      for (i in 1:length(bfyAnnotations)) {
        dataFrameAnnotation <- rbind(dataFrameAnnotation, bfyAnnotations[[i]])
      }
      # View(dataFrameAnnotation)
      # This is an arraylist of entries to check for duplicate results and nGrams
      nGrams <- list() #Elements of type BabelfyEntry
      
      for (i in 1:dim(dataFrameAnnotation)[i]) {
        
        row <- dataFrameAnnotation[i,]
        
        start <- row$charFragment.start + 1
        end <- row$charFragment.end + 1
        score <- row$globalScore
        synsetId <- row$babelSynsetID
        text <- substr(fixedText, start, end)

        if (length(nGrams) == 0) { # If this anotation is the first i have ever received
          nGrams <- list.append(nGrams, BabelfyEntry$new(start, end, score, synsetId, text))
          next
        }  
        
        # This is a sequential search to find previous synsets that are connected with
        # the current one
        pos <- 1
        prevAnot <- nGrams[[pos]]

        while (!(start >= prevAnot$getStartIdx() && end <= prevAnot$getEndIdx()) &&# The current anotation is
                                                                                   # included in other previous
                                                                                   # one
               !(prevAnot$getStartIdx() >= start && prevAnot$getEndIdx() <= end) && # A previous anotation is
                                                                                    # included in the current
                                                                                    # one
               pos < length(nGrams)
               ) {
          
          pos <- pos + 1
          prevAnot <- nGrams[[pos]]
        }

        if (start >= prevAnot$getStartIdx() && end <= prevAnot$getEndIdx()) { # The current anotation is included
                                                                              # in other previous one
          if (start == prevAnot$getStartIdx() && end == prevAnot$getEndIdx() && score > prevAnot$getScore()) {

            nGrams[[pos]] <- BabelfyEntry$new(start, end, score, synsetId, text)
            
          }
        } else {
          
          if (prevAnot$getStartIdx() >= start && prevAnot$getEndIdx() <= end) { # A previous anotation is
                                                                                # included in the current
                                                                                # one
            nGrams[[pos]] <- BabelfyEntry$new(start, end, score, synsetId, text)
            
          } else {
            
            nGrams <- list.append(nGrams, BabelfyEntry$new(start, end, score, synsetId, text)) # it is not related to nothing
                                                                                               # previous
          }
        } 
      }
      
      returnValue <- list()
      for (entry  in nGrams) {
        if (self$checkSynsetInBabelnet(entry$getSynsetId(), entry$getText())) {
          returnValue <- list.append(returnValue, entry$getText())
          names(returnValue)[length(returnValue)] <- entry$getSynsetId()
        }
      }
      # View(returnValue)
      return(returnValue)
    },
    
    getMaxBabelfyQuery = function() {
      #
      #Getter of MAX_BABELFY_QUERY variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of MAX_BABELFY_QUERY variable
      #
      return(private$MAX_BABELFY_QUERY)
    }
  ), 
  
  private = list(
    stopSynset = "bn:00031027n",
    MAX_BABELFY_QUERY = 3000,
    key = ""
  )
)
