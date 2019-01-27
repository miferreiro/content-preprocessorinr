#' Read a WARC entry from a WARC file
#'
#' Given the path to a WARC file (compressed or uncompressed) and the start
#' position of the WARC record, this function will produce an R object from the
#' WARC record.
#'
#' WARC \code{warinfo} objects are returned classed both \code{warc} and
#' \code{info}.
#'
#' WARC \code{response} objects are returned classed both \code{warc} and
#' \code{httr::response} and the standard \code{httr} content functions
#' will work with the object.
#'
#' WARC \code{request} objects are returned classed both \code{warc} and
#' \code{httr::request}.
#'
#' @param path path to WARC file
#' @param start starting offset of WARC record
#' @export
#' @note \code{warcinfo}, \code{request} and \code{response} objects are currently
#'   supported.
#' @examples \dontrun{
#' cdx <- read_cdx(system.file("extdata", "20160901.cdx", package="warc"))
#' i <- 1
#' path <- file.path(cdx$warc_path[i], cdx$file_name[i])
#' start <- cdx$compressed_arc_file_offset[i]
#'
#' (read_warc_entry(path, start))
#' }
read_warc_entry <- function(path, start, compressed=grepl(".gz$", path)) {
#cat("Start: ",start);
#cat("\n")
  path <- path.expand(path)


    fil <- file(path, "rb")
    seek(fil, start)

    # get content length
    cl <- 0
    repeat {
      line <- readLines(fil, 1)
	#  print(line)
      if (suppressWarnings(grepl("^Content-Length: ", line))) {
	  #Coge el contenido del campo Content-Length
        cl <- as.numeric(stri_split_fixed(trimws(line), ": ", 2)[[1]][2])
	#cat("Longitud cabecera(c1)2: ",cl,"\n"); #Longitud de la cabecera
        break
      }
    }

    # find end of WARC header
    repeat {
      line <- trimws(readLines(fil, 1))
      if (line == "") break
    }

    # go to end of record
    seek(fil, cl+2, "current")
	#cat("c1 2->>",cl+2,"\n")
    pos <- seek(fil)
	#cat("posicion final de la cabecera->>",pos,"\n")# posicion final de la cabecera
    seek(fil, start)
    buffer <- readBin(fil, "raw", pos - start)
    close(fil)

 # print(buffer)

  process_entry(buffer,pos)

}


