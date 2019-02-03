TeeCSVFromStringBufferPipe <- R6Class(
  
  "TeeCSVFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = ""){
      
      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instance, fileName = "propiedades.csv", withData = FALSE) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"character" %in% class(fileName)) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: fileName ", 
                  class(fileName))
      }  
      
      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: withData ", 
                  class(withData))
      }
      
      {  
        path <- instance$getPath()
        source <- paste(instance$getSource(),collapse = "")
        date <- instance$getDate()
        
        if (instance$isSpecificProperty("target")) {
          target <- instance$getSpecificProperty("target")
        } else {
          target <- ""
        }

        if (instance$isSpecificProperty("extension")) {
          extension <- instance$getSpecificProperty("extension")
        } else {
          extension <- ""
        }        

        if (instance$isSpecificProperty("length")) {
          length <- instance$getSpecificProperty("length")
        } else {
          length <- ""
        }   
  
        if (instance$isSpecificProperty("length_after_html_drop")) {
          length_after_html_drop <- instance$getSpecificProperty("length_after_html_drop")
        } else {
          length_after_html_drop <- ""
        } 
           
        if (instance$isSpecificProperty("@userName")) {
          userName <- paste0(unlist(instance$getSpecificProperty("@userName")), collapse = " ")
        } else {
          userName <- ""
        } 
           
        if (instance$isSpecificProperty("hashtag")) {
          hashtag <- paste0(unlist(instance$getSpecificProperty("hashtag")), collapse = " ")
        } else {
          hashtag <- ""
        } 
        
        if (instance$isSpecificProperty("URLs")) {
          URLs <- paste0(unlist(instance$getSpecificProperty("URLs")), collapse = " ")
        } else {
          URLs <- ""
        } 
        
        if (instance$isSpecificProperty("Emoticon")) {
          emoticon <- paste0(unlist(instance$getSpecificProperty("Emoticon")), collapse = " ")
        } else {
          emoticon <- ""
        }
        
        if (instance$isSpecificProperty("length_after_cleaning_text")) {
          length_after_cleaning_text <- paste0(unlist(instance$getSpecificProperty("length_after_cleaning_text")), collapse = " ")
        } else {
          length_after_cleaning_text <- ""
        } 
        
        if (instance$isSpecificProperty("language")) {
          language <- instance$getSpecificProperty("language") 
        } else {
          language <- ""
        } 

        
        if (instance$isSpecificProperty("abbreviation")) {
          abbreviation <- paste0(unlist(instance$getSpecificProperty("abbreviation")), collapse = " ")
        } else {
          abbreviation <- ""
        }       
        
        if (instance$isSpecificProperty("length_after_abbreviation")) {
          length_after_abbreviation <- instance$getSpecificProperty("length_after_abbreviation") 
        } else {
          length_after_abbreviation <- ""
        }
        
        if (instance$isSpecificProperty("langpropname")) {
          langpropname <- paste0(unlist(instance$getSpecificProperty("langpropname")), collapse = " ")
        } else {
          langpropname <- ""
        }
        
        if (instance$isSpecificProperty("length_after_slang")) {
          length_after_slang <- instance$getSpecificProperty("length_after_slang") 
        } else {
          length_after_slang <- ""
        }
        
        if (instance$isSpecificProperty("interjection")) {
          interjection <- paste0(unlist(instance$getSpecificProperty("interjection")), collapse = " ")
        } else {
          interjection <- ""
        }
        
        if (instance$isSpecificProperty("length_after_interjection")) {
          length_after_interjection <- instance$getSpecificProperty("length_after_interjection") 
        } else {
          length_after_interjection <- ""
        }
        
        if (instance$isSpecificProperty("stopWord")) {
          stopWord <- paste0(unlist(instance$getSpecificProperty("stopWord")), collapse = " ")
        } else {
          stopWord <- ""
        }
        
        if (instance$isSpecificProperty("length_after_stopwords")) {
          length_after_stopwords <- instance$getSpecificProperty("length_after_stopwords") 
        } else {
          length_after_stopwords <- ""
        }
      } 
      {
        row <-
          list(
            path,
            date,
            target,
            extension,
            length,
            length_after_html_drop,
            userName,
            hashtag,
            URLs,
            emoticon,
            length_after_cleaning_text,
            language,
            abbreviation,
            length_after_abbreviation,
            langpropname,
            length_after_slang,
            interjection,
            length_after_interjection,
            stopWord,
            length_after_stopwords,
            source
          )
      }
      {
        names(row) <-
          list(
            "path",
            "date",
            "target",
            "extension",
            "length",
            "length_after_html_drop",
            "userName",
            "hashtag",
            "URLs",
            "emoticon",
            "length_after_cleaning_text",
            "language",
            "abbreviation",
            "length_after_abbreviation",
            "langpropname",
            "length_after_slang",
            "interjection",
            "length_after_interjection",
            "stopWord",
            "length_after_stopwords",
            "source"
          )
      }
      
      if (withData) {
        row <- list.append(row, paste(instance$getData(),collapse = " "))
        names(row) <-
          list(
            "path",
            "date",
            "target",
            "extension",
            "length",
            "length_after_html_drop",
            "userName",
            "hashtag",
            "URLs",
            "emoticon",
            "length_after_cleaning_text",
            "language",
            "abbreviation",
            "length_after_abbreviation",
            "langpropname",
            "length_after_slang",
            "interjection",
            "length_after_interjection",
            "stopWord",
            "length_after_stopwords",
            "source",
            "data"
          )
      } 

      # write.table(rbindlist(list(row)), fileName, append = T, col.names = !file.exists(fileName),sep = ";", row.names = FALSE,qmethod  = c("double"))
      
      get_env(zz)$setDataFrameInstance(rbind(get_env(zz)$dataFrameInstance,rbindlist(list(row))))
      
      return(instance)
    }
  )
)

