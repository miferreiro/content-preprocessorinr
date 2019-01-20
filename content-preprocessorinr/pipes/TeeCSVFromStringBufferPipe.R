TeeCSVFromStringBufferPipe <- R6Class(
  
  "TeeCSVFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = ""){
      
      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instancia,fileName="propiedades.csv",withData = FALSE) {
      if (!"Instance" %in% class(instancia)) {
          stop("[TeeCSVFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
      }
      if (!"character" %in% class(fileName)) {
          stop("[TeeCSVFromStringBufferPipe][Error] Comprobacion del tipo de la variable fileName");
      }
      if (!"logical" %in% class(withData)) {
          stop("[TeeCSVFromStringBufferPipe][Error] Comprobacion del tipo de la variable fileName");
      }                                
      path <- instancia$getPath();
      source <- instancia$getSource();
      data <- instancia$getData();
      date <- instancia$getDate();
      propiedades <- instancia$getProperties();
      NombrePropiedades <- instancia$getNamesOfProperties();
    
      nombresColumnas <- c("Path","Source","Data","Date",NombrePropiedades)
      fila <- list(path,source,data,date,unlist(propiedades))
      #No funciona correctamente, se guardan en distintas filas
      if (!connections$checkFirstCsv()){
          
          fila <- data.frame(
              matrix(
                  unlist(fila),
                  ncol = length(nombresColumnas),
                  byrow = TRUE)
              , stringsAsFactors = FALSE)
          
          names(fila) <- nombresColumnas
          write.csv2(fila,file = fileName,append <- T,na ="Vacio",row.names = FALSE);
          
          connections$setCsvStatus(TRUE);
      }else{
          
          fila <- 
              matrix(
                  unlist(fila),
                  ncol = length(nombresColumnas),
                  byrow = TRUE)
             
          write.csv2(fila,file = fileName ,append = TRUE,na ="Vacio",row.names = FALSE,col.names = FALSE);
      }
    
 
    
      return(instancia);
    }
  )
)

