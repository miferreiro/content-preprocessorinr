#' @title bdp4R forward-pipe operator
#' @description Function that defines the operator that allows to take the flow
#' of the preprocessing of the instances.
#' @docType methods
#' @details This is the %>% operator of the modified magrittr library in order
#' to stop the flow when the instance is invalid.
#'
#' @param lhs An instance object.
#' @param rhs A function call using the bdp4R semantics.
#'
#' @usage lhs \%>|\% rhs
#'
#' @return The instance modified by the methods it has traversed.
#'
#' @seealso \code{\link{Instance}}
#'
#' @import magrittr
#' @export %>|%

`%>|%` <- function(lhs, rhs) {
  
  parent <- parent.frame()
  env <- new.env(parent = parent)
  chain_parts <- split_chain(match.call(), env = env)
  

  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]

  env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
                                                                               pipes[[i]], parent))

  
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {freduce(value, 
                                                                 `_function_list`)}), env, env), c("fseq", "function"))

  env[["freduce"]] <- freduce
  
  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  }
  else {
    
    env[["_lhs"]] <- eval(lhs, parent, parent)

    result <- withVisible(eval(expr = quote(`_fseq`(`_lhs`)),envir = env, 
                               enclos = env))
    if (is_compound_pipe(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent, 
           parent)
    }
    else {
      if (result[["visible"]]) 
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}
#
# @title Apply to the list of functions sequentially and control if the
#        instance is invalid
# @description This function applies the first function to value, then the next
#              function to the result of the previous function call, etc.
#              Execute the functions and allow interruption if the instance is
#              invalid.
# @details This is the freduce method of the modified magrittr library in order
#          to stop the flow when the instance is invalid.
# @docType methods
# @param value Initial value.
# @param function_list A list of functions.
#
# @return The result after applying each function in turn.
#
# @seealso \code{\link{Instance}}
#' @import magrittr
# @export freduce
#

freduce = function (value, function_list) 
{
  k <- length(function_list)

  if (k > 1) {
    for (i in 1:(k - 1L)) {
      value <- function_list[[i]](value)
      if (!value$isInstanceValid()) {
        cat("[pipeOperator][freduce][Info] The instance ", value$getPath(), 
            " is invalid and will not continue through the flow of pipes\n")
        break
      }
    }
  }

  if (value$isInstanceValid()) {
    value <- withVisible(function_list[[k]](value))
  } else {
    value <- withVisible(value)
  }


  if (value[["visible"]]) 
    value[["value"]]
  else invisible(value[["value"]])
}

split_chain = function (expr, env)
{
 
  rhss <- list()
  pipes <- list()
  i <- 1L


  while (is.call(expr) && is_pipe(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]

    
    if (is_parenthesized(rhs)){
       
      rhs <- eval(rhs, env, env)
    }
    
    
    rhss[[i]] <- if (is_dollar(pipes[[i]]) || is_funexpr(rhs))
      rhs
    else if (is_function(rhs))
      prepare_function(rhs)
    else if (is_first(rhs))
      prepare_first(rhs)
    else rhs
    

    
    if (is.call(rhss[[i]]) && identical(rhss[[i]][[1L]],
                                        quote(`function`)))
      stop("Anonymous functions myst be parenthesized",
           call. = FALSE)
    expr <- expr[[2L]]
    i <- i + 1L
  }

  
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}


is_pipe = function (pipe)
{
  identical(pipe, quote(`%>%`)) || identical(pipe, quote(`%>|%`)) || identical(pipe, quote(`%T>%`)) ||
    identical(pipe, quote(`%<>%`)) || identical(pipe, quote(`%$%`))
}

wrap_function = function (body, pipe, env)
{

  if (is_tee(pipe)) {
    body <- call("{", body, quote(.))
  }
  else if (is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  }
  
  eval(call("function", as.pairlist(alist(. = )), body), env,
       env)
}

is_tee = function (pipe)
{
  identical(pipe, quote(`%T>%`))
}

is_parenthesized = function (expr)
{
  is.call(expr) && identical(expr[[1]], quote(`(`))
}

is_dollar = function (pipe)
{
  identical(pipe, quote(`%$%`))
}

is_funexpr = function (expr)
{
  is.call(expr) && identical(expr[[1]], quote(`{`))
}

is_first = function (expr)
{
  !any(vapply(expr[-1], identical, logical(1), quote(.)))
}

prepare_first = function (expr)
{
  as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
}


is_placeholder = function (symbol)
{
  identical(symbol, quote(.))
}

is_compound_pipe = function (pipe)
{
  identical(pipe, quote(`%<>%`))
}

