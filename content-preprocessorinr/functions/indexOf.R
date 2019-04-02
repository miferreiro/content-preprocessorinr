
indexOf <- function(x,l, last=FALSE){
  x.vector <- sapply(1:nchar(x), function(i) substr(x, i, i))
  if (last) {
    return(ifelse(sum(x.vector == l) > 0, nchar(x) - which.max(x.vector[nchar(x):1] == l), -1))
  } else {
      return(ifelse(sum(x.vector == l) > 0, which.max(x.vector == l), -1))
  }
}

# vectorized function 
v.indexOf <- Vectorize(indexOf)