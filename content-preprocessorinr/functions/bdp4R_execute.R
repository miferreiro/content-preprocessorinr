#' @title Preproccess files through pipes
#' @description Method that allows to preprocess files in a comfortable way.
#' @docType methods
#' @param pathKeys (character) Path where the file with keys are located.
#' @param pathFiles (character) Path where the files to be preprocessed are located.
#' @param pipe (TypePipe) Subclass of TypePipe, which implements the pipe method.
#'
#' @usage bdp4R_execute(pathKeys, pathFiles, pipe = SerialPipes$new())
#'
#' @return List of instances that have been preprocessed.
#' @import streamR urltools backports
#' @importFrom purrr is_function
#'
#' @export bdp4R_execute
#'
bdp4R_execute = function(pathKeys = "content-preprocessorinr/config/configurations.ini",
                         pathFiles, 
                         pipe = SerialPipes$new()) {

  if (!"character" %in% class(pathKeys)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pathKeys ",
              class(pathKeys))
  }
  
  if (!"ini" %in% file_ext(pathKeys)) {
    stop("[bdp4R_execute][Error]
            Checking the extension of the file: pathKeys ",
              file_ext(pathKeys))
  }
  
  if (!"character" %in% class(pathFiles)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pathFiles ",
              class(pathFiles))
  }
  
  if (!"TypePipe" %in% class(pipe)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pipe ",
              class(pipe))
  }
  
  
  bdp4R_object <- Bdp4R$new(pathKeys)
  bdp4R_object$proccess_files(pathFiles, pipe = pipe)
}