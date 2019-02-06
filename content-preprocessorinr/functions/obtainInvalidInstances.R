obtainInvalidInstances = function(InstancesList, InvalidBooleanList) {
  
  if (!"list" %in% class(InstancesList)) {
    stop("[obtainValidInstances][Error] 
         Checking the type of the variable: InstancesList ", 
         class(InstancesList))
  }
  
  if (!"list" %in% class(InvalidBooleanList)) {
    stop("[obtainValidInstances][Error] 
         Checking the type of the variable: InvalidBooleanList ", 
         class(InvalidBooleanList))
  }
  
  cont = 1
  InvalidInstancesList <- list()
  for (elem in InstancesList) {
    if (!InvalidBooleanList[[cont]]) {
      InvalidInstancesList <- list.append(InvalidInstancesList, elem)
      names(InvalidInstancesList)[length(InvalidInstancesList)] <- names(InstancesList)[cont]
    }
    cont = cont + 1
  }
  rm(cont)
  return(InvalidInstancesList)
}