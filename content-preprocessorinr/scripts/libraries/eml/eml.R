
Read_In_Email = function(email_file, PartSelectedOnMPAlternative){
    email = new("Email", filename = email_file)
    email@PartSelectedOnMPAlternative = PartSelectedOnMPAlternative
    email = getElement(email)
    #email = Clean_Email_Input(email)

    return(email)
}
Clean_Email_Input <- function(email_object){
    
    #Taken from http://www.webmonkey.com/2008/08/four_regular_expressions_to_check_email_addresses/
    email_re <- "([a-z0-9][-a-z0-9_\\+\\.]*[a-z0-9])@([a-z0-9][-a-z0-9\\.]*[a-z0-9]\\.(arpa|root|aero|biz|cat|com|coop|edu|gov|info|int|jobs|mil|mobi|museum|name|net|org|pro|tel|travel|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|ee|eg|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)|([0-9]{1,3}\\.{3}[0-9]{1,3}))"
    
    #get jsut email address if it is available, otherwise preserve what was there
    temp <- stringr::str_extract(email_object@from,email_re)
    if(length(temp) > 0){
        email_object@from <- temp
    }
    
    #same thing for to field
    num_recipients <- length(email_object@to)
    for(i in 1:length(email_object@to)){
        temp <- stringr::str_extract(email_object@to[i],email_re)
        if(length(temp) > 0){
            email_object@to[i] <- temp
        }
    }
    
    #same thing for cc field
    for(i in 1:length(email_object@CC)){
        temp <- stringr::str_extract(email_object@CC[i],email_re)
        if(length(temp) > 0){
            email_object@CC[i] <- temp
            num_recipients <- num_recipients + 1
        }
    }
    #store number of recipeints 
    email_object@num_recipients <- num_recipients 
    
    Clean_String <- function(string){
        # string <- "inspections..#$^relocation..???!!!}{[]()"
        temp <- tolower(string)
        #remove everything that is not a number letter ? or !
        temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\d\\s:\\?\\!]", " ")
        # shrink down to just one white space
        temp <- stringr::str_replace_all(temp,"[\\d\\s]+", " ")
        #split it
        temp <- stringr::str_split(temp, " ")[[1]]
        #get rid of trailing "" if necessary
        indexes <- which(temp == "")
        if(length(indexes) > 0){
            temp <- temp[-indexes]
        }
        return(temp)
    }
    all_tokens <- 0 
    num_tokens <- 0
    
    #now deal with subject
    email_object@subject_tokenized <- Clean_String(email_object@subject)
    
    num_tokens <- num_tokens + length(email_object@subject_tokenized)
    all_tokens <- email_object@subject_tokenized
    
    #now deal with message 
    if(length(email_object@message) == 1){
        if(email_object@message == ""){
            email_object@message_tokenized <- ""
        }else{
            email_object@message_tokenized <- Clean_String(email_object@message)
            num_tokens <- num_tokens + length(email_object@message_tokenized)
            all_tokens <- append(all_tokens,email_object@message_tokenized)
        }  
    }else if(length(email_object@message) > 1){
        indexes <- which(email_object@message == "")
        if (length(indexes) > 0){
            email_object@message <- email_object@message[-indexes]
        }
        tokenized <- Clean_String(email_object@message[1])
        one_string <- email_object@message[1]
        for(i in 2:length(email_object@message)){
            tokenized <- append(tokenized, Clean_String(email_object@message[i]))
            one_string <- paste(one_string,email_object@message[i], sep = " ")
        }
        
        email_object@message <- one_string
        email_object@message_tokenized <- tokenized
        
        num_tokens <- num_tokens + length(email_object@message_tokenized)
        all_tokens <- append(all_tokens,email_object@message_tokenized)
    }
    
    email_object@num_tokens <- num_tokens 
    email_object@all_tokens <- all_tokens
    
    return(email_object)
    
}
#library(methods)
# defining a class Email which can be used to parse emails.

read_emails <- function(email_file, PartSelectedOnMPAlternative){
    
    if (class(email_file) != "character") {
        stop("email_files must be a character vector containing file paths to email txt files..." )
    }
    
    emails <- data.frame(message = "",
                         date = "",
                         filename = "",
                         stringsAsFactors = FALSE)
    email <- Read_In_Email(email_file, PartSelectedOnMPAlternative)
    
    return(email)
}
Email <- setClass(
    "Email",
    slots = list(
      other_elements = "vector",
      message = "character",
      date = "character",
      filename = "character",
      PartSelectedOnMPAlternative = "character"
    ),
    # Need to modify this validity function to ensure that the object is created only when filename is specified in the arguments
    validity = function(object){
        if (!file.exists(object@filename)) {
            return(paste("This file doesn't exist ",object@filename))
        }
    }
)
# Reserve method name getElement
setGeneric(name = "getElement",
           def = function(object,element){
               standardGeneric("getElement")
           }
)
# Define method getElement
setMethod(f = "getElement",
          signature = "Email",
          definition = function(object) {
              filename = object@filename
              PartSelectedOnMPAlternative = object@PartSelectedOnMPAlternative

              path <- paste("content-preprocessorinr/scripts", "parse.py", sep = "/")

              command <- paste("python", path, filename, "date", PartSelectedOnMPAlternative, sep = " ")
              try(suppressWarnings(response <- system(command, 
                                                      intern = T,
                                                      ignore.stderr = TRUE)), silent = T)
           
              if (!is.null(attr(response,"status"))) {
                  if (attr(response,"status") == 1) {
                      response <- ""
                  }
              }
              object@date = response
              

              command <- paste("python", path, filename, "message", PartSelectedOnMPAlternative, sep = " ")
              try(suppressWarnings(response <- system(command, 
                                                      intern = T,
                                                      ignore.stderr = TRUE)), silent = T)

              if (!is.null(attr(response,"status"))) {
                  if (attr(response,"status") == 1) {
                      response <- ""
                  }
              }
              object@message = response
             
              return(object)
          }
)
