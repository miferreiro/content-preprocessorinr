
BabelUtils <- R6Class(
  
  "BabelUtils",
  
  public = list(
    
    initialize = function() {
      private$key <- "YOUR-BABEL-KEY"
    },
    
    baseBabelNet = "https://babelnet.io/v5/",
    baseBalbelfy = "https://babelfy.io/v1/",
    # Determines whether a term is included in Babelnet or not
    # @param term The term to check
    # @param lang The language in which the term is written
    # @return true if the term is included in Babelnet ontological dictionary
    isTermInBabelNet = function(term, lang) {
      
      if (toupper(trim(lang)) %in% "UND") {
        cat("[BabelUtils][isTermInBabelNet][Error]","Unable to query Babelnet because language is not found.","\n")
        return(FALSE)
      }
      
      endpoint <- "getSynsetIds"
      call <-
        paste(self$baseBabelNet,
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
      
      cat("[BabelUtils][isTermInBabelNet][Info]","El termino: ", term," :", class(get_information_word) %in% "data.frame", "\n")
      return(class(get_information_word) %in% "data.frame")
    },
    #
    # Determines whether a synset is included in Babelnet or not
    #
    # @param synsetToCheck  The Synset to check
    # @param textToLink The word which corresponds with the Synset in Babelfy.
    #            This word is provided only to create a log report.
    # @return true if is possible to obtain information about synset in Babelnet,
    # so the synset is included in Babelnet ontological dictionary.
    # 
    checkSynsetInBabelnet = function(synsetToCheck, textToLink) {

        endpoint <- "getSynset"
        call <-
          paste(
            self$baseBabelNet,
            endpoint,
            "?",
            "id",
            "=",
            synsetToCheck,
            "&",
            "key",
            "=",
            private$key,
            sep = ""
          )
        get_information_synset <- GET(call)
        get_information_synset <- content(get_information_synset,"text")
        get_information_synset <- jsonlite::fromJSON(get_information_synset, flatten = TRUE)
        # print(get_information_synset)
        if (length(get_information_synset$senses) > 0) {
          cat("[BabelUtils][checkSynsetInBabelnet][Info]","The text [" , textToLink , "] obtained in Babelfy as [" , synsetToCheck , "] exists in Babelnet. ","\n")
          return(TRUE)
        } else {
          cat("[BabelUtils][checkSynsetInBabelnet][Error]","The text [" , textToLink , "] obtained in Babelfy as [" , synsetToCheck , "] does not exists in Babelnet. ","\n")
          return(FALSE)
        }
      
    },
    # 
    # Build a list of sysntets from a text
    # 
    # @param fixedText
    #            The text to be transformed into synsets
    # @param lang
    #            The language to identify the synsets
    # @return A vector of synsets. Each synset is represented in a pair (S,T) where
    #         S stands for the synset ID and T for the text that matches this
    #         synset ID
    # 
    buildSynsetVector = function(fixedText, lang) {

      remain <- fixedText
      parts <- list()
      # Se separa el texto en partes y se guardan en parts
      while (nchar(remain) > self$getMaxBabelfyQuery()) {
        segmet <- substr(remain, 0, self$getMaxBabelfyQuery)
        
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
      # print("Longitud de las partes")
      # print(length(parts))
      # View(parts)
      # Realiza las peticiones de cada parte y se guardan las consultas en bfyAnnotations
      # Falta controlar si se superan el numero de query establecida
      bfyAnnotations <- list()
      for (currentSubtext in 1:3002) {

          currentPart <- parts[[1]]
          endpoint <- "disambiguate"
          currentPart <- gsub("[[:space:]]", "+", currentPart)
          call <-
            paste(self$baseBalbelfy,
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
          
          if ("message" %in% names(get_disambiguate_text)) {
            print(get_disambiguate_text)
            cat("[BabelUtils][buildSynsetVector][Info]", get_disambiguate_text$message,"\n")
            
            now <- Sys.time() 
            
            midnight <- as.POSIXct(paste(as.character(Sys.Date() + 1), 
                                         "01:01:01", 
                                         sep = " "), 
                                  format = "%Y-%m-%d %H:%M:%S")
            
            diffHour <- unclass(midnight - now)[1]
            
            cat("[BabelUtils][buildSynsetVector][Info]", "Now ", as.POSIXct(now), "\n")
            cat("[BabelUtils][buildSynsetVector][Info]", "Midnight ", as.POSIXct(midnight), "\n")
            cat("[BabelUtils][buildSynsetVector][Info]", "Wait ", diffHour, " hours\n")
            
            Sys.sleep(diffHour * 60 * 60)
            
          } else {
            print(get_disambiguate_text)
            currentSubtext = currentSubtext + 1
            bfyAnnotations <- list.append(bfyAnnotations, get_disambiguate_text)
          }
      } 
      # Juntamos los data.frame de cada consulta almacenados en la lista bfyAnnotations
      dataFrameAnnotation <- data.frame()
      for (i in 1:length(bfyAnnotations)) {
        dataFrameAnnotation <- rbind(dataFrameAnnotation, bfyAnnotations[[i]])
      }
      # View(dataFrameAnnotation)
      # This is an arraylist of entries to check for duplicate results and nGrams
      nGrams <- list() #Elementos de tipo BabelfyEntry
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
                 !(prevAnot$getStartIdx() >= start && prevAnot$getEndIdx() >= end) && # A previous anotation is
                                                                              # included in the current
                                                                              # one
                 pos < length(nGrams)
                 ) {
            
            prevAnot <- nGrams[[pos]]
            pos <- pos + 1
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
      return(private$MAX_BABELFY_QUERY)
    }
  ), 
  
  private = list(
    stopSynset = "bn:00031027n",
    MAX_BABELFY_QUERY = 3000,
    key = ""
  )
)
