#Load packages
source("content-preprocessorinr/config/pkgChecker.R")
#Connections
source("content-preprocessorinr/config/Connections.R")
#Extractors
source("content-preprocessorinr/extractor/Instance.R")
source("content-preprocessorinr/extractor/ExtractorSms.R")
source("content-preprocessorinr/extractor/ExtractorTwtid.R")
source("content-preprocessorinr/extractor/ExtractorTtwt.R")
source("content-preprocessorinr/extractor/ExtractorWarc.R")
source("content-preprocessorinr/extractor/ExtractorEml.R")
source("content-preprocessorinr/extractor/ExtractorTytb.R")
source("content-preprocessorinr/extractor/ExtractorYtbid.R")
#Eml
source("content-preprocessorinr/scripts/libraries/eml/eml.R")
#InstanceFactory
source("content-preprocessorinr/functions/InstanceFactory.R")
#Pipes
source("content-preprocessorinr/pipes/TypePipe.R")
source("content-preprocessorinr/pipes/SerialPipes.R")
source("content-preprocessorinr/pipes/PipeGeneric.R")
source("content-preprocessorinr/pipes/TargetAssigningPipe.R")
source("content-preprocessorinr/pipes/File2Pipe.R")
source("content-preprocessorinr/pipes/GuessDatePipe.R")
source("content-preprocessorinr/pipes/StoreFileExtensionPipe.R")
source("content-preprocessorinr/pipes/MeasureLengthPipe.R")
source("content-preprocessorinr/pipes/StripHTMLPipe.R")
source("content-preprocessorinr/pipes/FindUserNamePipe.R")
source("content-preprocessorinr/pipes/FindHashtagPipe.R")
source("content-preprocessorinr/pipes/FindUrlPipe.R")
source("content-preprocessorinr/pipes/ToLowerCasePipe.R")
source("content-preprocessorinr/pipes/GuessLanguagePipe.R")
source("content-preprocessorinr/pipes/FindEmoticonPipe.R")
source("content-preprocessorinr/pipes/FindEmojiPipe.R")
source("content-preprocessorinr/pipes/AbbreviationPipe.R")
source("content-preprocessorinr/pipes/InterjectionPipe.R")
source("content-preprocessorinr/pipes/StopWordPipe.R")
source("content-preprocessorinr/pipes/TeeCSVPipe.R")
source("content-preprocessorinr/pipes/SlangPipe.R")
source("content-preprocessorinr/pipes/ContractionsPipe.R")
# source("content-preprocessorinr/pipes/NERFromStringBufferPipe.R")
# source("content-preprocessorinr/pipes/ComputePolarityFromStringBufferPipe.R")

#Pipe operator
source("content-preprocessorinr/functions/pipeOperator.R")
#read_warc override to get correctly the date 
source("content-preprocessorinr/functions/read_warc.R")
#proccess_files
source("content-preprocessorinr/functions/proccess_files.R")
#resource handle
source("content-preprocessorinr/functions/ResourceHandler.R")
#Types
source("content-preprocessorinr/types/TyposHandler.R")
source("content-preprocessorinr/types/UnmatchedTextHandler.R")
source("content-preprocessorinr/types/ObfuscationHandler.R")
source("content-preprocessorinr/types/UrbanDictionaryHandler.R")

source("content-preprocessorinr/types/SynsetVector.R")
source("content-preprocessorinr/types/SynsetFeatureVector.R")
source("content-preprocessorinr/types/SynsetDictionary.R")

#Utils 
source("content-preprocessorinr/utils/BabelfyEntry.R")
source("content-preprocessorinr/utils/BabelUtils.R")

source("content-preprocessorinr/pipes/StringBuffer2SynsetVectorPipe.R")
source("content-preprocessorinr/pipes/SynsetVector2SynsetFeatureVectorPipe.R")
source("content-preprocessorinr/pipes/TeeCSVFromSynsetFeatureVectorPipe.R")

source("content-preprocessorinr/functions/Bdp4R.R")
source("content-preprocessorinr/functions/bdp4R_execute.R")
