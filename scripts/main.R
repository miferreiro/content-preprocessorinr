#Carga todas las librerias y crea la lista de instancia inicial
{ 
archivosTest = "content-preprocessor/tests/www";
source("scripts/inicializacion.R")

#Construye el source y las propiedad de todos los objetos
invisible(sapply(listaInstancias,propiedadesTextoDate))
#Construye una lista de booleanos, donde TRUE es la posicion de la lista listaIntacias donde es válida, 
#FALSE, si no lo es
invalid <- lapply(listaInstancias,deleteInvalidInstances)
#Obtenemos la lista de instancais validas
listaInstanciasValidas <- obtainValidInstances()
#Se aplica la funcion de obtener propiedades Iniciales a las instancias validas
invisible(sapply(listaInstanciasValidas,propiedadesIniciales))

View(listaInstanciasValidas)
}

invisible(sapply(listaInstanciasValidas,pipes))

#Muestra las propiedades
for (x in listaInstanciasValidas) {
    print(x$getPath())
}

#Hacer csv
fun$toCsv(listaInstanciasValidas)

 rm(list=ls())
 # 
 # read_warc <- function(path, warc_types = NULL, include_payload = FALSE) {
 # 
 #     if (!is.null(warc_types)) {
 # 
 #         warc_types <- match.arg(
 #             warc_types,
 #             several.ok = TRUE,
 #             choices = c("warcinfo", "request", "response", "resource",
 #                         "metadata", "revisit", "conversion")
 #         )
 # 
 #     }
 # 
 #     path <- path.expand(path)
 # 
 #     if (!file.exists(path)) stop(sprintf('"%s" not found.', path, call.=FALSE))
 # 
 #    warc_obj <- new(J("is.rud.wrc.App"))
 #     
 #     warc_obj$process(path)
 # 
 #     suppressWarnings(
 #         data_frame(
 #             target_uri = warc_obj$warcTargetUriStr,
 #             ip_address = warc_obj$warcIpAddress,
 #             warc_content_type = warc_obj$contentTypeStr,
 #             warc_type = warc_obj$warcTypeStr,
 #             content_length = as.numeric(warc_obj$contentLengthStr),
 #             payload_type = warc_obj$warcIdentifiedPayloadTypeStr,
 #             profile = warc_obj$warcProfileStr,
 #             date = as.POSIXct(warc_obj$warcDateStr),
 #             http_status_code = as.numeric(warc_obj$httpStatusCode),
 #             http_protocol_content_type = warc_obj$httpProtocolContentType,
 #             http_version = warc_obj$httpVersion,
 #             http_raw_headers = lapply(warc_obj$httpRawHeaders, .jevalArray),
 #             warc_record_id = warc_obj$warcRecordIdStr
 #         )
 #     ) -> xdf
 # 
 #     if (include_payload) xdf$payload <- lapply(warc_obj$warc_payload, .jevalArray)
 # 
 #     if (!is.null(warc_types)) xdf <- dplyr::filter(xdf, warc_type %in% warc_types)
 # 
 #     xdf
 # 
 # }
 # .jinit()
 # read_warc("content-preprocessor/tests/www/_ham_/httpsduvi.uvigo.galduvi_glcontenidoartigosacademicas201803artigo_0012.html.warc")
 
#  install.packages("devtools")
#  library(devtools)
#  install_github("hrbrmstr/jwatr")
#      warc_file <- function(path, gzip=FALSE) {
# 
#      path <- path.expand(path)
#      path <- gsub("\\.warc[\\.gz]$", "", path)
#      path <- sprintf("%s.warc", path)
# 
#      list(
#          f = path,
#          wf = file(path, open="wb", raw=TRUE),
#          gzip = gzip
#      ) -> wobj
# 
#      class(wobj) <- "warc_file"
# 
#      wobj
# 
#  }
# zz <- warc_file("content-preprocessor/tests/www/_ham_/httpsduvi.uvigo.galduvi_glcontenidoartigosacademicas201803artigo_0012.html.warc")

# warc_GET <- function(wobj, url = NULL, config = list(), ..., handle = NULL) {
#     res <- httr::GET(url = url, config = config, handle = handle, ...)
#     warc_write_response(wobj, res)
#     invisible(res)
# }
# warc_GET(zz)
# read_warc <- function(path, warc_types = NULL, include_payload = FALSE) {
#     
#     if (!is.null(warc_types)) {
#         
#         warc_types <- match.arg(
#             warc_types,
#             several.ok = TRUE,
#             choices = c("warcinfo", "request", "response", "resource",
#                         "metadata", "revisit", "conversion")
#         )
#         
#     }
#     
#     path <- path.expand(path)
#     
#     if (!file.exists(path)) stop(sprintf('"%s" not found.', path, call.=FALSE))
#     
#     warc_obj <- new(J("is.rud.wrc.App"))
#     warc_obj$process(path)
#     
#     suppressWarnings(
#         data_frame(
#             target_uri = warc_obj$warcTargetUriStr,
#             ip_address = warc_obj$warcIpAddress,
#             warc_content_type = warc_obj$contentTypeStr,
#             warc_type = warc_obj$warcTypeStr,
#             content_length = as.numeric(warc_obj$contentLengthStr),
#             payload_type = warc_obj$warcIdentifiedPayloadTypeStr,
#             profile = warc_obj$warcProfileStr,
#             date = as.POSIXct(warc_obj$warcDateStr),
#             http_status_code = as.numeric(warc_obj$httpStatusCode),
#             http_protocol_content_type = warc_obj$httpProtocolContentType,
#             http_version = warc_obj$httpVersion,
#             http_raw_headers = lapply(warc_obj$httpRawHeaders, .jevalArray),
#             warc_record_id = warc_obj$warcRecordIdStr
#         )
#     ) -> xdf
#     
#     if (include_payload) xdf$payload <- lapply(warc_obj$warc_payload, .jevalArray)
#     
#     if (!is.null(warc_types)) xdf <- dplyr::filter(xdf, warc_type %in% warc_types)
#     
#     xdf
#     
# }
# .jinit()
# zzz <- read_warc("content-preprocessor/tests/www/_ham_/httpsduvi.uvigo.galduvi_glcontenidoartigosacademicas201803artigo_0012.html.warc") 
# 

 # read_warc_entry <- function (path, start, compressed = grepl(".gz$", path)) 
 # {
 #     path <- path.expand(path)
 #     if (compressed) {
 #         buffer <- sgzip_inflate_from_pos(path, start)
 #         if (is.null(buffer$result)) {
 #             buffer <- rwc_the_hard_way(path, start)
 #         }
 #         else {
 #             buffer <- buffer$result
 #         }
 #     }
 #     else {
 #         fil <- file(path, "rb")
 #         seek(fil, start)
 #         cl <- 0
 #         repeat {
 #             line <- readLines(fil, 1)
 #             if (suppressWarnings(grepl("^Content-Length: ", line))) {
 #                 cl <- as.numeric(stri_split_fixed(trimws(line), 
 #                                                   ": ", 2)[[1]][2])
 #                 break
 #             }
 #         }
 #         repeat {
 #             line <- trimws(readLines(fil, 1))
 #             if (line == "") 
 #                 break
 #         }
 #         seek(fil, cl + 2, "current")
 #         pos <- seek(fil)
 #         seek(fil, start)
 #         buffer <- readBin(fil, "raw", pos - start)
 #         close(fil)
 #     }
 #     process_entry(buffer)
 # }
 
 
#  library(warc)
#  arcAll <- list.files(path = "content-preprocessor/tests/www/_ham_"
#                       ,recursive = TRUE
#                       ,full.names = TRUE
#                       ,all.files = TRUE)
# 
# aa
# read_warc_entry(aa,19)
# #read.csv("./CSVs/outputFiles_13.11.18/output.csv")
# rm(csv)
# aux <- readLines(aa)
# View(aux)
# 
# library(sparkwarc)
# library(sparklyr)
# spark_uninstall()
 
 

 arcAll
 
 aa <- arcAll[1]

aa
{
{    library(stringi)
    library(pipeR)
    library(purrr)
source("scripts varios/warc-master/warc-master/R/aaa.r")
source("scripts varios/warc-master/warc-master/R/as_warc.r")
source("scripts varios/warc-master/warc-master/R/cdx.r")
source("scripts varios/warc-master/warc-master/R/create_cdx.r")
source("scripts varios/warc-master/warc-master/R/create_warc.r")
source("scripts varios/warc-master/warc-master/R/process_entry.r")
source("scripts varios/warc-master/warc-master/R/process_info.r")
source("scripts varios/warc-master/warc-master/R/process_request.r")
source("scripts varios/warc-master/warc-master/R/process_response.r")
source("scripts varios/warc-master/warc-master/R/RcppExports.R")
source("scripts varios/warc-master/warc-master/R/read_warc_entry.r")
source("scripts varios/warc-master/warc-master/R/utils.r")
source("scripts varios/warc-master/warc-master/R/validate.r")
source("scripts varios/warc-master/warc-master/R/warc-package.R")
source("scripts varios/warc-master/warc-master/R/write_warc_record.r")
bbb <- list()
num = 0
}
for ( i in 0:3){
    cat("\n")
    cat("-- ",i,"--",num,"\n")
    cat("\n")
    salida <- read_warc_entry(aa,num )
    num <- salida[["warc_header"]][["longitud"]]
    cat("Num: " , num)
    cat("\ncontent-length: " , salida[["warc_header"]][["content-length"]] )
    cat("\nLongitud: " , salida[["warc_header"]][["longitud"]])
    
    tipo <- salida[["warc_header"]][["WARC-Type"]]
    cat("\nTipo" ,tipo)
    cat("\n")
    if( equals(tipo,"response")  || equals(tipo,"request")){
        print(tipo)
        bbb <- list.append(bbb,tipo)
    }
}
}
     
 