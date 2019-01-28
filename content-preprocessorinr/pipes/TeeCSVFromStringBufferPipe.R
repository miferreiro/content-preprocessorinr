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
        date <- instance$getDate()
        target <- instance$getSpecificProperty("target")
        extension <- instance$getSpecificProperty("extension")
        length <- instance$getSpecificProperty("length")
        length_after_html_drop <- instance$getSpecificProperty("length_after_html_drop")
        userName <- paste0(unlist(instance$getSpecificProperty("userName")), collapse = " ")
        hashtag <- paste0(unlist(instance$getSpecificProperty("hashtag")), collapse = " ")
        URLs <- paste0(unlist(instance$getSpecificProperty("URLS")), collapse = " ")
        emoticon <- paste0(unlist(instance$getSpecificProperty("emoticon")), collapse = " ")
        length_after_cleaning_text <- instance$getSpecificProperty("length_after_cleaning_text")
        language <- instance$getSpecificProperty("language") 
        languageScore <- instance$getSpecificProperty("Language score") 
        languagePercent <- instance$getSpecificProperty("Language percent") 
        abbreviation <- paste0(unlist(instance$getSpecificProperty("abbreviation")), collapse = " ")
        length_after_abbreviation <- instance$getSpecificProperty("length_after_abbreviation") 
        langpropname <- paste0(unlist(instance$getSpecificProperty("langpropname")), collapse = " ")
        length_after_slang <- instance$getSpecificProperty("length_after_slang") 
        interjection <- paste0(unlist(instance$getSpecificProperty("interjection")), collapse = " ")
        length_after_interjection <- instance$getSpecificProperty("length_after_interjection") 
        stopWord <- paste0(unlist(instance$getSpecificProperty("stopWord")), collapse = " ")
        length_after_stopwords  <- instance$getSpecificProperty("length_after_stopwords") 
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
            languageScore,
            languagePercent,
            abbreviation,
            length_after_abbreviation,
            langpropname,
            length_after_slang,
            interjection,
            length_after_interjection,
            stopWord,
            length_after_stopwords
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
            "languageScore",
            "languagePercent",
            "abbreviation",
            "length_after_abbreviation",
            "langpropname",
            "length_after_slang",
            "interjection",
            "length_after_interjection",
            "stopWord",
            "length_after_stopwords"
          )
      }
      
      if (withData) {
        row <- list.append(row, instance$getData())
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
            "languageScore",
            "languagePercent",
            "abbreviation",
            "length_after_abbreviation",
            "langpropname",
            "length_after_slang",
            "interjection",
            "length_after_interjection",
            "stopWord",
            "length_after_stopwords",
            "data"
          )
      } 

      write.table(rbindlist(list(row)), fileName, append = T, col.names = !file.exists(fileName),sep = "  ")
        
      
      return(instance)
    }
  )
)

