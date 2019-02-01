read_warc = function (path, warc_types = NULL, include_payload = FALSE) 
{
  if (!is.null(warc_types)) {
    warc_types <- match.arg(warc_types, several.ok = TRUE, 
                            choices = c("warcinfo", "request", "response", "resource", 
                                        "metadata", "revisit", "conversion"))
  }
  path <- path.expand(path)
  if (!file.exists(path)) 
    stop(sprintf("\"%s\" not found.", path, call. = FALSE))
  warc_obj <- new(J("is.rud.wrc.App"))
  warc_obj$process(path)
  xdf <- suppressWarnings(data_frame(target_uri = warc_obj$warcTargetUriStr, 
                                     ip_address = warc_obj$warcIpAddress, warc_content_type = warc_obj$contentTypeStr, 
                                     warc_type = warc_obj$warcTypeStr, content_length = as.numeric(warc_obj$contentLengthStr), 
                                     payload_type = warc_obj$warcIdentifiedPayloadTypeStr, 
                                     profile = warc_obj$warcProfileStr, date = as.POSIXct(paste(substr(warc_obj$warcDateStr,1,10), substr(warc_obj$warcDateStr,12,19), sep = " " )), 
                                     http_status_code = as.numeric(warc_obj$httpStatusCode), 
                                     http_protocol_content_type = warc_obj$httpProtocolContentType, 
                                     http_version = warc_obj$httpVersion, http_raw_headers = lapply(warc_obj$httpRawHeaders, 
                                                                                                    .jevalArray), warc_record_id = warc_obj$warcRecordIdStr))
  
  if (include_payload) 
    xdf$payload <- lapply(warc_obj$warc_payload, .jevalArray)
  if (!is.null(warc_types)) 
    xdf <- dplyr::filter(xdf, warc_type %in% warc_types)
  xdf
}
