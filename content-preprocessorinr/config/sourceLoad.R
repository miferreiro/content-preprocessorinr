#Load packages
source("content-preprocessorinr/config/pkgChecker.R")
#Connections
source("content-preprocessorinr/config/connections.R")
#Extractors
source("content-preprocessorinr/extractor/Instance.R")
source("content-preprocessorinr/extractor/extractorSms.R")
source("content-preprocessorinr/extractor/extractorTwtid.R")
source("content-preprocessorinr/extractor/extractorTtwt.R")
source("content-preprocessorinr/extractor/extractorWarc.R")
source("content-preprocessorinr/extractor/extractorEml.R")
source("content-preprocessorinr/extractor/extractorTytb.R")
source("content-preprocessorinr/extractor/extractorYtbid.R")
#Eml
source("content-preprocessorinr/scripts/libraries/eml/eml.R")
#FactoryMethod
source("content-preprocessorinr/functions/FactoryMethod.R")
#Pipes
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
source("content-preprocessorinr/pipes/TeeCSVFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/SlangFromStringBufferPipe.R")
source("content-preprocessorinr/pipes/ContractionsFromStringBuffer.R")

#Pipe operator
source("content-preprocessorinr/functions/pipeOperator.R")
#read_warc override to get correctly the date 
source("content-preprocessorinr/functions/read_warc.R")
#proccess_files
source("content-preprocessorinr/functions/proccess_files.R")
#resource handle
source("content-preprocessorinr/functions/ResourceHandler.R")

