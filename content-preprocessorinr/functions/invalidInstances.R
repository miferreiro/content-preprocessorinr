deleteInvalidInstances = function(instance){
    if (!"Instance" %in% class(instance)){
        stop("[invalidInstances][Error] Comprobacion del tipo de la variable instance");
    }
    if ( is.null(instance) || is.null(instance$getSource())|| instance$getSource() == "" || instance$getSource() == "error" 
       || instance$getSpecificProperty("target") == "unrecognizable") {
        return(FALSE)
    }else{
        return(TRUE)
    }
}

obtainValidInstances = function(InstancesList,InvalidBooleanList){
    if (!"list" %in% class(InstancesList)){
        stop("[invalidInstances][Error] Comprobacion del tipo de la variable InstancesList");
    }
    if (!"list" %in% class(InstancesList)){
        stop("[invalidInstances][Error] Comprobacion del tipo de la variable InvalidBooleanList");
    }
    
    cont = 1;
    for(elem in InstancesList){
        if(InvalidBooleanList[[cont]]){
            ValidInstancesList <- list.append(ValidInstancesList,elem);
            names(ValidInstancesList)[length(ValidInstancesList)] <- names(InstancesList)[cont];
        }
        cont = cont + 1;
    }
    rm(cont)
    return(ValidInstancesList);
}