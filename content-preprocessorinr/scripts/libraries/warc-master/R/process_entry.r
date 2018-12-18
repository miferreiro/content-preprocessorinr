process_entry <- function(buffer,pos2) {

  pos <- find_sequence(buffer, charToRaw("\r\n\r\n"))
 # cat("find_sequence(buffer, charToRaw(\r\n\r\n)): ",pos)
 # cat("\n")
  warc_info <- buffer[1:(pos-1)]

  warc_info <- rawToChar(warc_info)
# print(warc_info)
  warc_entries <- stri_split_regex(warc_info, "\r\n")[[1]]
   # cat("\n")
    # cat("\n")
	#cat("--warc_entries[2]--: ", warc_entries[2])
	 # cat("\n")
	  # cat("\n")
  # cat(substr(warc_entries[1], 1, 5))
  # if (substr(warc_entries[1], 1, 5) != "WARC/") {
  # warning("Invalid WARC record")
  # return(NULL)
  # }

  stri_split_fixed(warc_entries[-1], ": ", 2) %>%
    map(~setNames(list(.[2]), .[1])) %>%
    flatten_df() -> warc_headers


  warc_headers <- suppressMessages(readr::type_convert(warc_headers))
  warc_headers <- as.list(warc_headers)
  warc_headers <- httr::insensitive(warc_headers)

 # warc_headers <- map("Longitud registro", 100)
  #print("---")
  warc_headers["longitud"] <- pos2
  #print(warc_headers)
 #print("---")
  if (length(warc_headers[["warc-target-uri"]]) > 0) {
    warc_headers$`warc-target-uri` <-
      remove_brackets(warc_headers$`warc-target-uri`)
  }

  warc_type <- tolower(warc_headers[["warc-type"]])

  if (warc_type == "response") {

    start <- pos+4
    size <- warc_headers$`Content-Length`[1]

    response <- buffer[start:length(buffer)]

    return(process_response(response, warc_headers))

  } else if (warc_type == "warcinfo") {

    start <- pos+4
    size <- warc_headers$`Content-Length`[1]

    info <- buffer[start:length(buffer)]

    return(process_info(info, warc_headers))

  } else if (warc_type == "request") {

    start <- pos+4
    size <- warc_headers$`Content-Length`[1]

    request <- buffer[start:length(buffer)]

    return(process_request(request, warc_headers))

  }

  ret <- list(warc_header=warc_headers)

  class(ret) <- c("warc", class(ret))

  return(ret)


}


# 'warcinfo', 'response', , 'request', 'resource', 'metadata',
# 'revisit', 'conversion', 'continuation'