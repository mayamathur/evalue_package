#' @title Calculate a bound for the bias
#'
#' @description Function used to calculate the maximum factor by which a risk
#' ratio is biased, given possible values for each of the parameters that
#' describe the bias factors for each type of bias.
#'
#' @param biases A set of biases (or single bias) to include in the calculation
#'   of the bound. A single object constructed with the [multi_bias()] function,
#'   it may include any or all of [confounding()], [selection()], and
#'   [misclassification()], and any of the options described in the
#'   documentation for those functions.
#' @param RRAUc Named parameter values with which to calculate a bound. Names must
#'   correspond to the parameters defining the biases provided by `biases`. Help
#'   with names can be found by running `print(multi_bias(...))` for the biases
#'   of interest. Unnecessary parameters are ignored with a warning.
#' @param RRUcY See `RRAUc`
#' @param RRUsYA1 See `RRAUc`
#' @param RRSUsA1 See `RRAUc`
#' @param RRUsYA0 See `RRAUc`
#' @param RRSUsA0 See `RRAUc`
#' @param RRAUscS See `RRAUc`
#' @param RRUscYS See `RRAUc`
#' @param RRAYy See `RRAUc`
#' @param ORYAa See `RRAUc`
#' @param RRYAa See `RRAUc`
#' @param RRAYyS See `RRAUc`
#' @param ORYAaS See `RRAUc`
#' @param RRYAaS See `RRAUc`
#' @param RRAUsS See `RRAUc`
#' @param RRUsYS See `RRAUc`
#' @return Returns the value of the bound formed as a function of the provided
#'   parameters.
#' @details The names of the parameters in the bound can be found for a given
#'   set of biases with `print(biases)`. Running `summary(biases)` shows the
#'   equivalent notation used in the output of the [multi_evalue()] function.
#' @keywords multi-bias
#'
#' @examples
#' multi_bound(multi_bias(confounding()),
#'             RRAUc = 2.2, RRUcY = 1.7)
#'
#' biases <- multi_bias(confounding(), selection("S = U"),
#'                      misclassification("exposure",
#'                      rare_outcome = TRUE, rare_exposure = FALSE))
#'
#' print(biases)
#'
#' multi_bound(biases,
#'             RRAUc = 3, RRUcY = 2, RRSUsA1 = 2.3,
#'             RRSUsA0 = 1.7, ORYAaS = 5.2)
#'
#' @export
#' @importFrom methods formalArgs

multi_bound <- function(biases, RRAUc = NULL, RRUcY = NULL, RRUsYA1 = NULL, 
                        RRSUsA1 = NULL, RRUsYA0 = NULL, RRSUsA0 = NULL, 
                        RRAUscS = NULL, RRUscYS = NULL, RRAYy = NULL, ORYAa = NULL, 
                        RRYAa = NULL, RRAYyS = NULL, ORYAaS = NULL, RRYAaS = NULL,
                        RRAUsS = NULL, RRUsYS = NULL) {
  
  if (!inherits(biases, c("bias", "multi_bias"))) {
    stop('Argument "biases" must be of class "bias" or "multi_bias"')
  }
 
  if (class(biases) == "bias") biases <- multi_bias(biases)
  SU <- any(unlist(sapply(biases, attr, which = "SU")))
  
  
  params <- mget(formalArgs(multi_bound)[-1])
  params[sapply(params, is.null)] <- NULL
  
  necc_params <- attr(biases, "parameters")
  
  if (!all(necc_params$argument %in% names(params))) {
    missing_params <- setdiff(necc_params$argument, names(params))
    err_mess <- paste0(
      "You are missing parameters necessary to calculate a bound,",
      " or they have missing or incorrect names.",
      " You need to supply the following additional arguments: ",
      paste0(paste0(missing_params, collapse = " = , "), " ="), 
      ". Run summary(biases) for more information on the arguments."
    )
    stop(err_mess)
  }
  
  if (length(names(params)) > length(necc_params$argument)) {
    extra_params <- setdiff(names(params), necc_params$argument)
    warning(paste0(
      "You seem to have supplied uncessary parameters (",
      paste0(extra_params, collapse = ", "), "). ",
      "Check to make sure you have chosen the appropriate biases. ",
      "These are the parameters that are being used: ",
      paste0(necc_params$argument, collapse = ", "), "."
    ))
  }
  
  necc_params$vals <- NA
  for (i in seq_len(nrow(necc_params))) {
    necc_params$vals[i] <- unlist(params[necc_params$argument[i]])
  }
  
  conf_vals <- necc_params$vals[grep("confounding", necc_params$bias)]
  miscl_vals <- necc_params$vals[grep("misclassification", necc_params$bias)]
  sel1_vals <- necc_params$vals[necc_params$bias == "selection" &
                                  grepl("1", necc_params$latex)]
  sel0_vals <- necc_params$vals[necc_params$bias == "selection" &
                                  grepl("0", necc_params$latex)]
  
  # just go backwards so that bf_func returns the val!
  if (SU && length(sel1_vals) == 1) sel1_vals <- c(threshold(sel1_vals), threshold(sel1_vals))
  if (SU && length(sel0_vals) == 1) sel0_vals <- c(threshold(sel0_vals), threshold(sel0_vals))
  
  # add on a 1 to selection values if it's only length 1,
  # which will make it easy to use the rr function no matter what
  if (length(sel1_vals) == 1) sel1_vals <- c(sel1_vals, 1)
  if (length(sel0_vals) == 1) sel0_vals <- c(sel0_vals, 1)
  
  conf_prod <- if (length(conf_vals) > 1) bf_func(conf_vals[1], conf_vals[2]) else 1
  miscl_prod <- if (length(miscl_vals) > 0) miscl_vals else 1
  sel1_prod <- if (length(sel1_vals) > 1) bf_func(sel1_vals[1], sel1_vals[2]) else 1
  sel0_prod <- if (length(sel0_vals) > 1) bf_func(sel0_vals[1], sel0_vals[2]) else 1
  
  return(conf_prod * miscl_prod * sel1_prod * sel0_prod)
}


