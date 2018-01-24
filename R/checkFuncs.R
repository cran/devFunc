# The next 3 functions are inherited by the 'ArgumentCheck' package, which is deprecated
# and not necessarily available on CRAN, however, I like the next 3 functions such that
# I've just added them here, unaltered.

newArgCheck <- function() {
  argcheck <- new.env()
  assign("n_warn", 0, envir = argcheck)
  assign("warn_msg", NULL, envir = argcheck)
  assign("n_error", 0, envir = argcheck)
  assign("error_msg", NULL, envir = argcheck)
  assign("n_message", 0, envir = argcheck)
  assign("message_msg", NULL, envir = argcheck)
  class(argcheck) <- c("ArgCheck", "environment")
  return(argcheck)
}

addError <- function(msg, argcheck){

  if (!"ArgCheck" %in% class(argcheck))
    stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_error", get("n_error", envir = argcheck) + 1, envir = argcheck)
  assign("error_msg", c(get("error_msg", envir = argcheck), msg), envir = argcheck)
}

finishArgCheck <- function(argcheck){

  fn_call <- sys.call(-1)
  fn_call <- utils::capture.output(fn_call)
  if (!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  argcheck <- mget(ls(envir = argcheck), envir = argcheck)
  if (argcheck$n_warn > 0)
    warning(paste0(c("", fn_call, paste0(1:argcheck$n_warn, ": ", argcheck$warn_msg)), collapse = "\n"), call. = FALSE)
  if (argcheck$n_message > 0)
    message(paste0(c("", fn_call, paste0(1:argcheck$n_message, ": ", argcheck$message_msg)), collapse = "\n"))
  if (argcheck$n_error > 0)
    stop(paste0(c("", fn_call, paste0(1:argcheck$n_error, ": ", argcheck$error_msg)), collapse = "\n"), call. = FALSE)
}

#' Checking if all elements of a list are all logical vectors
#'
#' @param listLogic A list of the vectors of which one wishes to check if their data type is logical
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#' @return No value is returned if all vectors have the logical data type. If not, an error message is thrown for each element of the list that does not pertain to the logical data type.
#' @examples
#' arg1 <- TRUE
#' checkLogicVec(list(arg1))
#'
#' \donttest{checkLogicVec(list(TRUE, T, 2))
#' checkLogicVec(list(TRUE, T, 2), c('Var1', 'Var2', 'Var3'))
#'
#' arg2 <- 0.8
#' checkLogicVec(list(arg2))
#' checkLogicVec(list(arg2, 'T', 2))}

checkLogicVec <- function(listLogic, namesListElements = NULL){

  if(!is.list(listLogic)) stop("The argument 'listLogic' needs to be a list.")

  for(iList in 1:length(listLogic)){
    if(is.list(listLogic[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }

  checkList <- newArgCheck()
  argNames <- deparse(substitute(listLogic))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))

  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listLogic) != length(namesListElements)) stop("The argument 'listLogic' should have the same length as argument 'namesListElements'.")
  }

  if(is.list(listLogic)){
    for(iList in 1:length(listLogic)){
      if(!is.logical(listLogic[[iList]]) & !is.null(listLogic[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The data type of argument '", argNames[iList], sep = ''), "' is NOT a logical vector.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a logical vector.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The data type of argument '", namesListElements[iList], sep = ''), "' is NOT a logical vector.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listLogic) & length(listLogic) == 1){
    if(!is.logical(listLogic) & !is.null(listLogic)){
      if(isObject){
        addError(msg = paste(paste("The data type of argument '", argNames, sep = ''), "' is NOT a logical vector.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The data type of element with index ', '1', sep = ''), ' is NOT a logical vector', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The data type of argument '", namesListElements, sep = ''), "' is NOT a logical vector.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listLogic" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}

#' Checking if all elements of a list are all numeric vectors
#'
#' @param listNum A list of the vectors of which one wishes to check if their data type is numeric
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#' @return No value is returned if all vectors have the numeric data type. If not, an error message is thrown for each element of the list that does not pertain to the numeric data type.
#' @examples
#' arg1 <- 2
#' checkNumVec(list(arg1))
#'
#' \donttest{checkNumVec(list(TRUE, T, 2))
#' checkNumVec(list(TRUE, T, 2), c('Var1', 'Var2', 'Var3'))
#'
#' arg2 <- 0.8
#' checkNumVec(list(arg2))
#' checkNumVec(list(arg2, 'T', 2))}

checkNumVec <- function(listNum, namesListElements = NULL){

  if(!is.list(listNum)) stop("The argument 'listNum' needs to be a list.")

  for(iList in 1:length(listNum)){
    if(is.list(listNum[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }

  checkList <- newArgCheck()
  argNames <- deparse(substitute(listNum))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))

  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listNum) != length(namesListElements)) stop("The argument 'listNum' should have the same length as argument 'namesListElements'.")
  }

  if(is.list(listNum)){
    for(iList in 1:length(listNum)){
      if(!is.numeric(listNum[[iList]]) & !is.null(listNum[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The data type of argument '", argNames[iList], sep = ''), "' is NOT a numeric vector.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a numeric vector.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The data type of argument '", namesListElements[iList], sep = ''), "' is NOT a numeric vector.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listNum) & length(listNum) == 1){
    if(!is.numeric(listNum) & !is.null(listNum)){
      if(isObject){
        addError(msg = paste(paste("The data type of argument '", argNames, sep = ''), "' is NOT a numeric vector.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The data type of element with index ', '1', sep = ''), ' is NOT a numeric vector', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The data type of argument '", namesListElements, sep = ''), "' is NOT a numeric vector.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listNum" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}

#' Checking if all elements of a list are all integer vectors
#'
#' @param listInt A list of the vectors of which one wishes to check if their data type is integer
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#' @return No value is returned if all vectors have the integer data type. If not, an error message is thrown for each element of the list that does not pertain to the integer data type.
#' @examples
#' arg1 <- 1L
#' checkIntVec(list(arg1))
#'
#' \donttest{checkIntVec(list(1L, TRUE, 2L))
#'
#' arg2 <- 'R'
#' checkIntVec(list(arg2))
#' checkIntVec(list(arg2, TRUE, 2))}

checkIntVec <- function(listInt, namesListElements = NULL){

  if(!is.list(listInt)) stop("The argument 'listInt' needs to be a list.")

  for(iList in 1:length(listInt)){
    if(is.list(listInt[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }

  checkList <- newArgCheck()
  argNames <- deparse(substitute(listInt))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))

  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listInt) != length(namesListElements)) stop("The argument 'listInt' should have the same length as argument 'namesListElements'.")
  }

  if(is.list(listInt)){
    for(iList in 1:length(listInt)){
      if(!is.integer(listInt[[iList]]) & !is.null(listInt[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The data type of argument '", argNames[iList], sep = ''), "' is NOT a integer vector.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a integer vector.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The data type of argument '", namesListElements[iList], sep = ''), "' is NOT a integer vector.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listInt) & length(listInt) == 1){
    if(!is.integer(listInt) & !is.null(listInt)){
      if(isObject){
        addError(msg = paste(paste("The data type of argument '", argNames, sep = ''), "' is NOT a integer vector.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The data type of element with index ', '1', sep = ''), ' is NOT a integer vector', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The data type of argument '", namesListElements, sep = ''), "' is NOT a integer vector.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listInt" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}

#' Checking if all elements of a list are all integer or numeric vectors
#'
#' @param listNumOrInt A list of the vectors of which one wishes to check if their data type is integer.
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#' @return No value is returned if all vectors have the integer or numeric data type. If not, an error message is thrown for each element of the list that does not pertain to the integer or numeric data type.
#' @examples
#' arg1 <- 1L
#' checkNumOrIntVec(list(arg1))
#'
#' arg1 <- 1
#' checkNumOrIntVec(list(arg1))
#'
#' \donttest{checkNumOrIntVec(list(1L, TRUE, 2L))
#' checkNumOrIntVec(list(1L, TRUE, 2L), c('Var1', 'Var2', 'Var3'))
#'
#' arg2 <- 'R'
#' checkNumOrIntVec(list(arg2))
#' checkNumOrIntVec(list(arg2, TRUE, 2))}

checkNumOrIntVec <- function(listNumOrInt, namesListElements = NULL){

  if(!is.list(listNumOrInt)) stop("The argument 'listNumOrInt' needs to be a list.")

  for(iList in 1:length(listNumOrInt)){
    if(is.list(listNumOrInt[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }

  checkList <- newArgCheck()
  argNames <- deparse(substitute(listNumOrInt))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))

  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listNumOrInt) != length(namesListElements)) stop("The argument 'listNumOrInt' should have the same length as argument 'namesListElements'.")
  }

  if(is.list(listNumOrInt)){
    for(iList in 1:length(listNumOrInt)){
      if(!is.numeric(listNumOrInt[[iList]]) & !is.integer(listNumOrInt[[iList]]) & !is.null(listNumOrInt[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The data type of argument '", argNames[iList], sep = ''), "' is NOT integer nor numeric.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT integer nor numeric.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The data type of argument '", namesListElements[iList], sep = ''), "' is NOT integer nor numeric.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listNumOrInt) & length(listNumOrInt) == 1){
    if(!is.numeric(listNumOrInt) & !is.integer(listNumOrInt) & !is.null(listNumOrInt)){
      if(isObject){
        addError(msg = paste(paste("The data type of argument '", argNames, sep = ''), "' is NOT integer nor numeric.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The data type of element with index ', '1', sep = ''), ' is NOT integer nor numeric', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The data type of argument '", namesListElements, sep = ''), "' is NOT integer nor numeric.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listNumOrInt" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}

#' Checking if all elements of a list are all character vectors
#'
#' @param listChar A list of the vectors of which one wishes to check if their data type is character
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#' @return No value is returned if all vectors have the character data type. If not, an error message is thrown for each element of the list that does not pertain to the character data type.
#' @examples
#' arg1 <- 'something'
#' checkCharVec(list(arg1))
#'
#' \donttest{checkCharVec(list('somethingElse', TRUE))
#'
#' arg2 <- 2
#' checkCharVec(list(arg2))
#' checkCharVec(list(arg2, TRUE, 5L))}

checkCharVec <- function(listChar, namesListElements = NULL){

  if(!is.list(listChar)) stop("The argument 'listChar' needs to be a list.")

  for(iList in 1:length(listChar)){
    if(is.list(listChar[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }

  checkList <- newArgCheck()
  argNames <- deparse(substitute(listChar))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))

  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listChar) != length(namesListElements)) stop("The argument 'listChar' should have the same length as argument 'namesListElements'.")
  }

  if(is.list(listChar)){
    for(iList in 1:length(listChar)){
      if(!is.character(listChar[[iList]]) & !is.null(listChar[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The data type of argument '", argNames[iList], sep = ''), "' is NOT a string/character vector.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a string/character vector.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The data type of argument '", namesListElements[iList], sep = ''), "' is NOT a string/character vector.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listChar) & length(listChar) == 1){
    if(!is.character(listChar) & !is.null(listChar)){
      if(isObject){
        addError(msg = paste(paste("The data type of argument '", argNames, sep = ''), "' is NOT a string/character vector.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The data type of element with index ', '1', sep = ''), ' is NOT a string/character vector', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The data type of argument '", namesListElements, sep = ''), "' is NOT a string/character vector.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listChar" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}

#' Checking if the length of the different elements of a list corresponds to what one expects.
#'
#' @param listObjects List of vectors, of irrespective data type.
#' @param lengthObjects Numeric vector, either of the same length as the 'listObjects' argument, or of length 1, but in the latter case, it will be tested whether or not the length of every element of the 'listObjects' argument equal this one value.
#' @return No value is returned if all vectors correspond to the length against which it is tested. An error message is thrown when the length does not corresponds for at least one element of the list.
#' @examples
#' arg1 <- 'something'
#' checkLength(list(arg1), 1)
#'
#' checkLength(list('somethingElse', TRUE), 1)
#' checkLength(list('somethingElse', TRUE), c(1, 1))
#'
#' arg2 <- 2:5
#' checkLength(list(arg1, arg2), c(1, 4))
#' \donttest{checkLength(list(arg1, arg2), 1)}

checkLength <- function(listObjects, lengthObjects){

  namesListObjects <- deparse(substitute(listObjects))
  if(str_count(namesListObjects, "list[(]") > 1){
    stop('All the objects to be checked need to be put in a list. A list of lists is not permitted by this function.')
  }
  if(nargs() == 1){
    stop("A value for the 'lengthObjects' argument needs to be provided for.")
  }
  if(!(length(lengthObjects) == 1 | length(listObjects) == length(lengthObjects))){
    stop("The length of the 'lengthObjects' argument should be equal to the length of the 'listObjects' argument, or equal to 1. ")
  }
  if(length(lengthObjects) == 1 & length(listObjects) > 1){
    lengthObjects <- rep(lengthObjects, length(listObjects))
  }

  namesListObjects <- unlist(strsplit(namesListObjects, '[,]'))
  namesListObjects <- gsub("\\s", "", namesListObjects)
  namesListObjects <- str_replace_all(namesListObjects, fixed("list("), "")
  namesListObjects <- str_replace_all(namesListObjects, fixed(")"), "")

  if(is.list(lengthObjects)){
    checkNumVec(lengthObjects)
  } else {
    checkNumVec(as.list(lengthObjects))
  }
  checkList <- newArgCheck()

  for(iList in 1:length(listObjects)){
    if(length(listObjects[[iList]]) != lengthObjects[iList]){
      addError(msg = paste(paste(paste('The length of argument ', namesListObjects[[iList]], sep = ''), ' does NOT correspond to ', sep = ''), lengthObjects[iList], sep = ''), argcheck = checkList)
    }
  }
  finishArgCheck(checkList)
}

#' Checking if the value of vectors (of length 1) is authorized.
#'
#' @param listObjects List of vectors, of irrespective data type and each of length 1. It contains the list of variables of which one wants to test its value against a vector of valid values. This argument is obligatory.
#' @param listValues List of vectors, of irrespective data type and of the same length as the 'listObjects' argument. It contains the values against which one wants to test the 'listObjects' argument. This argument is obligatory.
#' @return No value is returned if all vectors correspond to the length against which it is tested. An error message is thrown when at least one of the elements of the 'listObjects' contains an invalid value, as stipulated by the 'listValues' argument.
#' @examples
#'lossType <- 'absolute'
#'checkValues(list(lossType), list(c('absolute', 'quadratic')))
#'\donttest{checkValues(list(lossType), list(c('absolute', 'quadratic'), c('test', 'test2')))}
#'
#'#The next error message is weird, since it does not return the real name of the listObject
#'#that found to be wrong.
#'lossType <- 'absolute55'
#'listObjects <- list(lossType)
#'listValues <- list(c('absolute', 'quadratic'))
#'\donttest{checkValues(listObjects, listValues)
#'
#'#Now it is ok...
#'checkValues(list(lossType), list(c('absolute', 'quadratic')))}

checkValues <- function(listObjects, listValues){

  namesListObjects <- deparse(substitute(listObjects))
  if(str_count(namesListObjects, "list[(]") > 1){
    stop("All the objects of the 'listObjects' argument need to be put in a list. A list of lists is not permitted by this function.")
  }
  namesListValues <- deparse(substitute(listValues))
  if(str_count(namesListValues, "list[(]") > 1){
    stop("All the values of the 'listValues' argument need to be put in a list. A list of lists is not permitted by this function.")
  }
  if(nargs() == 1){
    stop("A value for the 'listValues' argument needs to be provided for.")
  }
  if(length(listObjects) != length(listValues)){
    stop("The length of the 'lengthObjects' argument should be equal to the length of the 'listValues' argument. ")
  }

  namesListObjects <- unlist(strsplit(namesListObjects, '[,]'))
  namesListObjects <- gsub("\\s", "", namesListObjects)
  namesListObjects <- str_replace_all(namesListObjects, fixed("list("), "")
  namesListObjects <- str_replace_all(namesListObjects, fixed(")"), "")

  checkList <- newArgCheck()
  for(iList in 1:length(listObjects)){
    if(length(listObjects[[iList]]) > 1){
      addError(msg = paste(paste('The length of argument ', namesListObjects[[iList]], sep = ''), ' should not exceed 1.', sep = ''), argcheck = checkList)
    }
    if(!listObjects[[iList]] %in% listValues[[iList]]){
      if(length(listValues[[iList]]) == 1) returnedValue <- listValues[[iList]]
      if(length(listValues[[iList]]) == 2) returnedValue <- paste(listValues[[iList]], collapse = ' and ')
      if(length(listValues[[iList]]) > 2) returnedValue <- paste(paste(listValues[[iList]][1:(length(listValues[[iList]]) - 1)], collapse = ', '), listValues[[iList]][length(listValues[[iList]])], sep = ' and ')
      addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not part of any of the permitted values: ', sep = ''), returnedValue, sep = ''), argcheck = checkList)
    }
  }
  finishArgCheck(checkList)
}

#' Checking if the value of a numeric or integer variable (of length 1) is located within a certain range.
#'
#' @param listObjects List of numeric or integer vectors, of each of length 1. It contains the list of variables of which one wants to test its value against a vector of valid values. This argument is obligatory.
#' @param listRanges List of character vectors, each character vector should be of length 2 or 4, while the 'listRanges' list should be of the same length as the 'listObjects' argument. It contains the values against which one wants to test the 'listObjects' argument. This argument is obligatory.
#' @return No value is returned if all vectors of the 'listObjects' argument is contained within the corresponding ranges of the 'listRanges' argument. An error message is thrown when this is not the case for at least one of the elements of the 'listObjects' argument. Note that each element of the 'listRange' argument should be of the following structure. The first element of the character vector, as well as the third element if the character vector is of length 4, should either be '>', '>=', '<' or '<='. In case that the length of the character vector is 4, the first and the third element should be opposite directions (some form of '>' combined with some form of '<'). The second and fourth element should be a numeric value coerced to a character. If the character vector is of length 2 (4), then the range is either bounded from below or (and) above.
#' @examples
#'someValue <- 2
#'checkRanges(list(someValue), list(c('<', 3)))
#'
#'\donttest{someValue <- '2'
#'checkRanges(list(someValue), list(c('<', 3)))
#'checkRanges(list(someValue), list(c(1.5, 3)))}
#'
#'someValue <- 6
#'someOtherValue <- 5
#'checkRanges(list(someValue, someOtherValue), list(c('>=', 2.5), c('>=', 2.5, '<=', 5)))
#'\donttest{checkRanges(list(someValue, someOtherValue), list(c('>=', 2.5), c('>=', 2.5, '<', 5)))
#'checkRanges(list(someValue, someOtherValue), list(c('>=', 2.5, '<=', 5), c('>=', 2.5, '<', 5)))}

checkRanges <- function(listObjects, listRanges){

  namesListObjects <- deparse(substitute(listObjects))
  if(str_count(namesListObjects, "list[(]") > 1){
    stop("All the objects of the 'listObjects' argument need to be put in a list. A list of lists is not permitted by this function.")
  }
  nameslistRanges <- deparse(substitute(listRanges))
  if(str_count(nameslistRanges, "list[(]") > 1){
    stop("All the values of the 'listRanges' argument need to be put in a list. A list of lists is not permitted by this function.")
  }
  if(nargs() == 1){
    stop("A value for the 'listRanges' argument needs to be provided for.")
  }
  if(length(listObjects) != length(listRanges)){
    stop("The length of the 'lengthObjects' argument should be equal to the length of the 'listRanges' argument. ")
  }

  namesListObjects <- unlist(strsplit(namesListObjects, '[,]'))
  namesListObjects <- gsub("\\s", "", namesListObjects)
  namesListObjects <- str_replace_all(namesListObjects, fixed("list("), "")
  namesListObjects <- str_replace_all(namesListObjects, fixed(")"), "")

  checkList <- newArgCheck()
  for(iList in 1:length(listObjects)){
    if(length(listObjects[[iList]]) > 1){
      addError(msg = paste(paste('The length of argument ', namesListObjects[[iList]], sep = ''), ' should not exceed 1.', sep = ''), argcheck = checkList)
    }
    checkNumOrIntVec(list(listObjects[[iList]]), namesListObjects[[iList]])
    checkCharVec(list(listRanges[[iList]]), sprintf('listRange_Element_%d', iList))

    if(!(length(listRanges[[iList]]) %in% c(2, 4))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to be of length 2 or 4.", iList), argcheck = checkList)
    if(!(listRanges[[iList]][1] %in% c('>', '>=', '<', '<='))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a '>', '>=', '<', '<=' sign at index 1 of this character vector.", iList), argcheck = checkList)
    if(is.na(as.numeric(listRanges[[iList]][2])))  addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a numeric value at index 2 of this character vector.", iList), argcheck = checkList)

    if(length(listRanges[[iList]]) == 4){
      if(!(listRanges[[iList]][3] %in% c('>', '>=', '<', '<='))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a '>', '>=', '<', '<=' sign at index 3 of this character vector.", iList), argcheck = checkList)
      if(is.na(as.numeric(listRanges[[iList]][4])))  addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a numeric value at index 4 of this character vector.", iList), argcheck = checkList)

      if(as.numeric(listRanges[[iList]][2]) > as.numeric(listRanges[[iList]][4])){
        temp <- listRanges[[iList]][1:2]
        listRanges[[iList]][1:2] <- listRanges[[iList]][3:4]
        listRanges[[iList]][3:4] <- temp
        if(!(listRanges[[iList]][1] %in% c('>', '>='))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a '>', '>=' sign at index 3 of this character vector, elsewise the range definition does not make much sense.", iList), argcheck = checkList)
        if(!(listRanges[[iList]][3] %in% c('<', '<='))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a '<', '<=' sign at index 1 of this character vector, elsewise the range definition does not make much sense.", iList), argcheck = checkList)
      } else{
        if(!(listRanges[[iList]][1] %in% c('>', '>='))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a '>', '>=' sign at index 1 of this character vector, elsewise the range definition does not make much sense.", iList), argcheck = checkList)
        if(!(listRanges[[iList]][3] %in% c('<', '<='))) addError(msg = sprintf("Element %s of the 'listRanges' argument needs to contain a '<', '<=' sign at index 3 of this character vector, elsewise the range definition does not make much sense.", iList), argcheck = checkList)

        if(listRanges[[iList]][1] == '>'){
          if(!listObjects[[iList]] > as.numeric(listRanges[[iList]][2])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not > ', sep = ''), listRanges[[iList]][2], sep = ''), argcheck = checkList)
        } else if(listRanges[[iList]][1] == '>='){
          if(!listObjects[[iList]] >= as.numeric(listRanges[[iList]][2])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not >= ', sep = ''), listRanges[[iList]][2], sep = ''), argcheck = checkList)
        }
        if(listRanges[[iList]][3] == '<'){
          if(!listObjects[[iList]] < as.numeric(listRanges[[iList]][4])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not < ', sep = ''), listRanges[[iList]][4], sep = ''), argcheck = checkList)
        } else if(listRanges[[iList]][3] == '<='){
          if(!listObjects[[iList]] <= as.numeric(listRanges[[iList]][4])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not <= ', sep = ''), listRanges[[iList]][4], sep = ''), argcheck = checkList)
        }
      }
    } else {
      if(listRanges[[iList]][1] == '>'){
        if(!listObjects[[iList]] > as.numeric(listRanges[[iList]][2])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not > ', sep = ''), listRanges[[iList]][2], sep = ''), argcheck = checkList)
      } else if(listRanges[[iList]][1] == '>='){
        if(!listObjects[[iList]] >= as.numeric(listRanges[[iList]][2])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not >= ', sep = ''), listRanges[[iList]][2], sep = ''), argcheck = checkList)
      } else if(listRanges[[iList]][1] == '<'){
        if(!listObjects[[iList]] < as.numeric(listRanges[[iList]][2])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not < ', sep = ''), listRanges[[iList]][2], sep = ''), argcheck = checkList)
      } else if(listRanges[[iList]][1] == '<='){
        if(!listObjects[[iList]] <= as.numeric(listRanges[[iList]][2])) addError(msg = paste(paste(paste('The value of argument ', namesListObjects[[iList]], sep = ''), ' is not <= ', sep = ''), listRanges[[iList]][2], sep = ''), argcheck = checkList)
      }
    }
  }
  finishArgCheck(checkList)
}
