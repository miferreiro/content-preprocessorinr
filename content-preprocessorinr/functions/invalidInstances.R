deleteInvalidInstances = function(instance) {
  
    if (!"Instance" %in% class(instance)) {
      stop("[invalidInstances][Error] 
              Checking the type of the variable: instance ", 
                class(instance))
    }
  
    if ( is.null(instance) || !instance$isInstanceValid()) {
      return(FALSE)
    } else {
      return(TRUE)
    }
}

obtainValidInstances = function(InstancesList, InvalidBooleanList) {
  
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
    
    for (elem in InstancesList) {
        if (InvalidBooleanList[[cont]]) {
            ValidInstancesList <- list.append(ValidInstancesList, elem)
            names(ValidInstancesList)[length(ValidInstancesList)] <- names(InstancesList)[cont]
        }
        cont = cont + 1
    }
    rm(cont)
    return(ValidInstancesList)
}

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