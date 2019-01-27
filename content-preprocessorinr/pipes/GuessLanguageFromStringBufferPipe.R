GuessLanguageFromStringBufferPipe <- R6Class(
    
  "GuessLanguageFromStringBufferPipe",
 
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "language") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[GuessLanguageFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    pipe = function(instance, languageTwitter = TRUE) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[GuessLanguageFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      if (languageTwitter 
            && instance$getSpecificProperty("extension") %in% "twtid") {

        if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                                "tweets/_", 
                                  instance$getSpecificProperty("target"), 
                                    "_/", 
                                      instance$getId(), 
                                        ".json", 
                                          sep = ""))) {
          
          path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
                          instance$getSpecificProperty("target"), 
                            "_/", 
                              instance$getId(), 
                                ".json", 
                                  sep = "")
          
          dataFromJsonFile <- fromJSON(file = path)
          
          if (!is.na(dataFromJsonFile[["lang"]]) && 
                !is.null(dataFromJsonFile[["lang"]]) 
                  && dataFromJsonFile[["lang"]] != "") {
            
            langTwitter <- dataFromJsonFile[["lang"]]
            
            instance$addProperties(langTwitter,super$getPropertyName())
            
            if (is.null(instance$getSpecificProperty("language"))) {
              message <- c( "The file: " , instance$getPath() , " has a NULL twitter language")
              warning(message)    
              instance$addProperties(message, "reasonToInvalidate") 
              instance$invalidate()
              return(instance)
            }
            
            instance$addProperties(0,"Language score")
          
            instance$addProperties(0,"Language percent")
            
            return(instance)
          } 
        }
      } 
      
      instance$getData() %>>% 
        self$getLanguage() %>>%
          {instance$addProperties(.,super$getPropertyName())}
      
      if ( is.null(instance$getSpecificProperty("language"))) {
        message <- c( "The file: " , instance$getPath() , " has a null language")
        warning(message)    
        instance$addProperties(message, "reasonToInvalidate") 
        instance$invalidate()
        return(instance)
      }
      
      instance$getData() %>>% 
        self$getLanguageScore() %>>%
          {instance$addProperties(.,"Language score")}
      
      instance$getData() %>>% 
        self$getLanguagePercent() %>>%
          {instance$addProperties(.,"Language percent")} 
         
      
      
      return(instance)
    },
    
    getLanguage = function(data) {
      
      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguage][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      lang <- detectLanguage(data, isPlainText = TRUE)[[1]][[1]]
      
      langStandardize <- self$getLanguageStandardized(lang)
      
      return(langStandardize)
    },
    
    getLanguageScore = function(data) {

      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguageScore][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(detectLanguage(data)[[7]])  
    },
    
    getLanguagePercent = function(data) {

      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguagePercent][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(detectLanguage(data)[[10]])  
    },
    
    getLanguageStandardized = function(lang) {
      
      if (!"character" %in% class(lang)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguageStandardized][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }      
      
      hash <- list(
        
       AFAR  = "aa",
       ABKHAZIAN  = "ab",
        # = "ae",
       AFRIKAANS     = "af",
        # = "ak",
        # = "am",
        # = "an",
       ARABIC      = "ar",
        # = "as",
        # = "av",
       AYMARA      = "ay",
        # = "az",
       BASKIR     = "ba",
       BELARUSIAN     = "be",
       BULGARIAN     = "bg",
       BISLAMA    = "bh",
        # = "bi",
        # = "bm",
       BENGALI    = "bn",
       TIBETAN    = "bo",
       BRETON    = "br",
       BOSNIAN    = "bs",
       CATALAN    = "ca",
        # = "ce",
         # = "ch",
       CORSICAN     = "co",
        # = "cr",
       CZECH    = "cs",
        # = "cu",
        # = "cv",
       WELSH    = "cy",
       DANISH    = "da",
       GERMAN    = "de",
       DHIVEHI    = "dv",
       DZONGKHA    = "dz",
        # = "ee",
       GREEK   = "el",
       ENGLISH   = "en",
       ESPERANTO   = "eo",
       SPANISH    = "es",
       ESTONIAN   = "et",
       BASQUE   = "eu",
       PERSIAN   = "fa",
        # = "ff",
       FINNISH   = "fi",
       FIJIAN  = "fj",
        # = "fo",
       FRENCH   = "fr",
       FRISIAN   = "fy",
       IRISH   = "ga",
       SCOTS_GAELIC   = "gd",
       GALICIAN  = "gl",
       GUARANI  = "gn",
       GUJARATI   = "gu",
       MANX  = "gv",
       HAUSA   = "ha",
       HEBREW  = "he",
       HINDI  = "hi",
        # = "ho",
       CROATIAN  = "hr",
       HAITIAN_CREOLE  = "ht",
       HUNGARIAN  = "hu",
       ARMENIAN  = "hy",
         # = "hz",
       INTERLINGUA  = "ia",
       INDONESIAN    = "id",
      # = "ie",
        # = "ig",
        # = "ii",
        # = "ik",
        # = "io",
        ICELANDIC = "is",
        ITALIAN  = "it",
        INUKTITUT  = "iu",
        JAPANESE  = "ja",
        JAVANESE  = "jv",
        GEORGIAN  = "ka",
        # = "kg",
        # = "ki",
        # = "kj",
        KAZAKH  = "kk",
        GREENLANDIC  = "kl",
        # = "km",
        # = "kn",
        KOREAN = "ko",
        # = "kr",
        # = "ks",
        KURDISH  = "ku",
        # = "kv",
        # = "kw",
        # = "ky",
        LATIN  = "la",
        LUXEMBOURGISH  = "lb",
        # = "lg",
        # = "li",
        LINGALA  = "ln",
        # = "lo",
        LITHUANIAN  = "lt",
        # = "lu",
        LATVIAN = "lv",
        MALGASY = "mg",
       # = "mh",
        MAORI = "mi",
        MACEDONIAN = "mk",
        MALAYALAM = "ml",
        MONGOLIAN = "mn",
        MARATHI = "mr",
        MALAY = "ms",
        MALTESE = "mt",
        BURMESE = "my",
        NAURU = "na",
        # = "nb",
        # = "nd",
        NEPALI = "ne",
        # = "ng",
        DUTCH = "nl",
        # = "nn",
        NORWEGIAN = "no",
        # = "nr",
        # = "nv",
        # = "ny",
        OCCITAN = "oc",
        # = "oj",
        OROMO= "om",
        ORIYA = "or",
        # = "os",
        # = "pa",
        # = "pi",
        POLISH = "pl",
        PASTHO = "ps",
        PORTUGUESE_p = "pt",
        QUECHUA = "qu",
        RHAETO_ROMANCE = "rm",
        # = "rn",
        ROMANIAN = "ro",
        RUSSIAN = "ru",
        KINYARWANDA = "rw",
        SANSKRIT = "sa",
        # = "sc",
        SINDHI = "sd",
        # = "se",
        SANGO = "sg",
        SINHALESE = "si",
        SLOVAK = "sk",
        SLOVENIAN = "sl",
        SAMOAN = "sm",
        SHONA = "sn",
        SOMALI = "so",
        ALBANIAN = "sq",
        SERBIAN = "sr",
        # = "ss",
        SESOTHO = "st",
        SUNDANESE = "su",
        SWEDISH = "sv",
        SWAHILI = "sw",
        TAMIL = "ta",
        TELUGU = "te",
        # = "tg",
        THAI = "th",
        TIGRINYA = "ti",
        TURKMEN = "tk",
        TAGALOG = "tl",
        # = "tn",
        TONGA = "to",
        TURKISH = "tr",
        TSONGA = "ts",
        TATAR = "tt",
        TWI = "tw",
        # = "ty",
        UIGHUR = "ug",
        UKRAINIAN = "uk",
        URDU = "ur",
        UZBEK = "uz",
        # = "ve",
        VIETNAMESE = "vi",
        VOLAPUK = "vo",
        # = "wa",
        WOLOF = "wo",
        # = "xh",
        YIDDISH = "yi",
        YORUBA = "yo",
        ZHUANG = "za",
        CHINESE  = "zh",
        ZULU  = "zu"
        
                   )
      
      langStandardized <- hash[[lang]]
      
      return(langStandardized)

    }
    
  )  
)
