#CARGA DE PAQUETES
source("content-preprocessorinr/config/pkgChecker.R")
#CONEXIONES
source("content-preprocessorinr/config/connections.R")
#EXTRACTOR
source("content-preprocessorinr/extractor/Instance.R")
source("content-preprocessorinr/extractor/extractorSms.R")
source("content-preprocessorinr/extractor/extractorTwtid.R")
source("content-preprocessorinr/extractor/extractorTtwt.R")
source("content-preprocessorinr/extractor/extractorWarc.R")
source("content-preprocessorinr/extractor/extractorEml.R")
source("content-preprocessorinr/extractor/extractorTytb.R")
source("content-preprocessorinr/extractor/extractorYtbid.R")
#FactoryMethod
source("content-preprocessorinr/functions/FactoryMethod.R")
#INVALID
source("content-preprocessorinr/functions/deleteInvalidInstances.R")
source("content-preprocessorinr/functions/obtainInvalidInstances.R")
source("content-preprocessorinr/functions/obtainValidInstances.R")
#EML
source("content-preprocessorinr/scripts/libraries/eml/eml.R")
#WARC
# source("content-preprocessorinr/scripts/libraries/warc-master/R/process_entry.r")
# source("content-preprocessorinr/scripts/libraries/warc-master/R/process_info.r")
# source("content-preprocessorinr/scripts/libraries/warc-master/R/process_request.r")
# source("content-preprocessorinr/scripts/libraries/warc-master/R/process_response.r")
# source("content-preprocessorinr/scripts/libraries/warc-master/R/read_warc_entry.r")
#PIPES
source("content-preprocessorinr/pipes/TypePipe.R")
source("content-preprocessorinr/pipes/SerialPipes.R")
source("content-preprocessorinr/pipes/PipeGeneric.R")
source("content-preprocessorinr/pipes/TargetAssigningFromPathPipe.R")
source("content-preprocessorinr/pipes/File2StringBufferPipe.R")
source("content-preprocessorinr/pipes/GuessDateFromFilePipe.R")
source("content-preprocessorinr/pipes/StoreFileExtensionPipe.R")
source("content-preprocessorinr/pipes/MeasureLengthFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/StripHTMLFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/FindUserNameInStringBufferPipe.R")
source("content-preprocessorinr/pipes/FindHashtagInStringBufferPipe.R")
source("content-preprocessorinr/pipes/FindUrlInStringBufferPipe.R")
source("content-preprocessorinr/pipes/StringBufferToLowerCasePipe.R")
source("content-preprocessorinr/pipes/GuessLanguageFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/FindEmoticonInStringBufferPipe.R")
source("content-preprocessorinr/pipes/FindEmojiInStringBufferPipe.R")
source("content-preprocessorinr/pipes/AbbreviationFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/InterjectionFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/StopWordFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/ComputePolarityFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/NERFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/TeeCSVFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/StringBuffer2SynsetVectorPipe.R")
source("content-preprocessorinr/pipes/SlangFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/ContractionsFromStringBuffer.R")
#PIPE OPERATOR
source("content-preprocessorinr/functions/pipeOperator.R")
#read_warc sobreescrito para que coja la hora correctamente
source("content-preprocessorinr/functions/read_warc.R")
#proccess_files
source("content-preprocessorinr/functions/proccess_files.R")
#resource handle
source("content-preprocessorinr/functions/ResourceHandler.R")
