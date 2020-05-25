#'@title Unmeasured confounding
#'
#'@description A type of bias. Declares that unmeasured confounding will be a
#'component of interest in the multi-bias sensitivity analysis. Generally used
#'within other functions, its output is returned invisibly.
#'
#'@param ... Other arguments. Not currently used for this function.
#'@param verbose Logical. If `TRUE`, returns warnings and messages immediately.
#'  Defaults to `FALSE` because it is generally used within the [multi_bias()]
#'  function, which will print the same messages/warnings.
#'@return Invisibly returns a list with components `n` (2, the degree of the
#'  polynomial in the numerator), `d` (1, the degree of the polynomial in the
#'  denominator), `mess` (any messages/warnings that should be printed for the
#'  user), and `bias` ("confounding").
#'@keywords multi-bias
#' @examples
#' # returns invisibly without print()
#' print(confounding())
#'
#' # Calculate an E-value for unmeasured confounding only
#' multi_evalue(est = RR(4), biases = confounding())
#'
#'@export

confounding <- function(..., verbose = FALSE) {
  arguments <- c(...)
  mess <- NULL

  if (!is.null(arguments)) {
    mess <- "Additional options are not currently accepted for confounding() and have been ignored."
  }

  if (!is.null(mess) & verbose) wrapmessage(mess)
  
  obj <- paste(gsub("\\\"", "'", deparse(sys.call())), collapse = " ")
  
  names(obj) <- "confounding"
  class(obj) <- "bias"
  attr(obj, "n") <- 2
  attr(obj, "d") <- 1
  attr(obj, "message") <- mess

  invisible(obj)
}



#' @title Selection bias
#'
#' @description A type of bias. Declares that selection bias will be a component
#' of interest in the multi-bias sensitivity analysis. Generally used within
#' other functions; its output is returned invisibly.
#'
#' @param ... Optional arguments describing the type of potential selection
#'   bias. Options are "general" (general selection bias, the default if no
#'   options are chosen), "increased risk" and "decreased risk" (assumptions
#'   about the direction of risk in the selected population), "S = U"
#'   (simplification used if the biasing characteristic is common to the entire
#'   selected population), and "selected" (when the target of inference is the
#'   selected population only). Errors are produced when incompatible
#'   assumptions are chosen.
#' @param verbose Logical. If `TRUE`, returns warnings and messages immediately.
#'   Defaults to `FALSE` because it is generally used within the [multi_bias()]
#'   function, which will print the same messages/warnings.
#' @return Invisibly returns a list with components whose values depend on the
#'   options chosen: `n` (the degree of the polynomial in the numerator), `d`
#'   (the degree of the polynomial in the denominator),`mess` (any
#'   messages/warnings that should be printed for the user), and
#'   `bias`("selection").
#'
#' @keywords multi-bias
#'
#' @examples
#' # returns invisibly without print()
#' print(selection("general", "increased risk"))
#'
#' # Calculate an E-value for selection bias only
#' multi_evalue(est = RR(4),
#'          biases = selection("general", "increased risk"))
#'
#' @export

selection <- function(..., verbose = FALSE) {
  arguments <- c(...)
  mess <- NULL

  # if nothing chosen, assume general selection bias with no assumptions
  if (is.null(arguments)) {
    arguments <- "general"
    mess <- "The default option, general selection bias, is being used."
  }

  SU_match <- grep("(s|S)\\s*\\=\\s*(u|U)", arguments)
  arguments[SU_match] <- "S = U"
  
  type <- match.arg(arguments,
                    c("general", "selected", "S = U", "increased risk", "decreased risk"),
                    several.ok = TRUE
  )

  if (length(arguments) > length(type)) {
    nomatch <- setdiff(arguments, type)
    mess <- paste(mess,
                  paste0("\"", nomatch, "\" is/are not valid options for selection bias and have been ignored."),
                  sep = "\n"
    )
  }

  # if selected population has been chosen as well as any other option
  # return with error
  if ("selected" %in% type && length(type) > 1) {
    stop("These assumptions are incompatible; choose \"general\" instead of \"selected\".")
  }

  if ("increased risk" %in% type && "decreased risk" %in% type) {
    stop("These assumptions are incompatible; choose either increased or decreased risk.")
  }

  # if neither target population was selected
  if (!"general" %in% type && !"selected" %in% type) {
    type <- c(type, "general")
    mess <- paste(mess, "The default option, general selection bias, is being used as well.",
                  sep = "\n"
    )
  }

  poss_args <- list(
    "general" = list(n = 4, d = 2),
    "selected" = list(n = 2, d = 1),
    "S = U" = list(n = -2, d = -2),
    "increased risk" = list(n = -2, d = -1),
    "decreased risk" = list(n = -2, d = -1)
    )

  # find the degrees in the numerator and denominator
  n <- Reduce(
    f = "+",
    sapply(poss_args[type], function(x) x$n)
  )
  d <- Reduce(
    f = "+",
    sapply(poss_args[type], function(x) x$d)
  )
  # if both increased/decreased risk and S = U chosen, then there's an overlap
  # in degrees taken away, so add back in
  if ("S = U" %in% type && ("increased risk" %in% type || "decreased risk" %in% type)) {
    n <- n + 1
    d <- d + 1
  }

  # remove any leading line breaks just for aesthetics
  if (!is.null(mess)) {
    if (substr(mess, 1, 1) == "\n") mess <- substr(mess, 2, nchar(mess))
  }

  if (verbose & !is.null(mess)) wrapmessage(mess)
  
  obj <- paste(gsub("\\\"", "'", deparse(sys.call())), collapse = " ")
  
  names(obj) <- "selection"
  class(obj) <- "bias"
  attr(obj, "n") <- n
  attr(obj, "d") <- d
  attr(obj, "message") <- mess
  attr(obj, "selected") <- "selected" %in% type
  attr(obj, "increased_risk") <- "increased risk" %in% type
  attr(obj, "decreased_risk") <- "decreased risk" %in% type
  attr(obj, "SU") <- "S = U" %in% type
  
  invisible(obj)
}


#'@title Misclassification
#'
#'@description A type of bias. Declares that (differential) misclassification
#'will be a component of interest in the multi-bias sensitivity analysis.
#'Generally used within other functions; its output is returned invisibly.
#'
#'@param ... Arguments describing the type of misclassification. Currently two
#'  options: "outcome" or "exposure".
#'@param rare_outcome Logical. Is the outcome rare enough that outcome odds
#'  ratios approximate risk ratios? Only needed when considering exposure
#'  misclassification. Note that `rare_outcome = FALSE` returns an error, as
#'  this option is not currently available.
#'@param rare_exposure Logical. Is the exposure rare enough that exposure odds
#'  ratios approximate risk ratios? Only needed when considering exposure
#'  misclassification.
#'@param verbose Logical. If `TRUE`, returns warnings and messages immediately.
#'  Defaults to `FALSE` because it is generally used within the [multi_bias()]
#'  function, which will print the same messages/warnings.
#'
#'@return Invisibly returns a list with components whose values depend on the
#'  options chosen: `n` (the degree of the polynomial in the numerator), `d`
#'  (the degree of the polynomial in the denominator), `m` (the parameters in
#'  the bias factor), `mess` (any messages/warnings that should be printed for
#'  the user), and `bias`("misclassification").
#'
#'@keywords multi-bias
#'
#' @examples
#' # returns invisibly without print()
#' print(misclassification("outcome"))
#'
#' # Calculate an E-value for misclassification
#' multi_evalue(est = RR(4),
#'          biases = misclassification("exposure",
#'                   rare_outcome = TRUE, rare_exposure = TRUE))
#'
#'@export


misclassification <- function(...,
                              rare_outcome = FALSE, rare_exposure = FALSE,
                              verbose = FALSE) {
  arguments <- c(...)
  mess <- NULL
  
  if (is.null(arguments)) {
    stop("Either \"outcome\" or \"exposure\" must be chosen as an option.")
  }
  
  type <- match.arg(arguments,
                    c("outcome", "exposure"),
                    several.ok = TRUE
  )
  
  if (length(arguments) > length(type)) {
    nomatch <- setdiff(arguments, type)
    mess <- paste(mess,
                  paste0("'", nomatch, "' is/are not valid options for missclassification and have been ignored."),
                  sep = "\n"
    )
  }
  
  if ("outcome" %in% type && "exposure" %in% type) {
    stop("Only one of either \"exposure\" or \"outcome\" can be chosen.")
  }
  
  if (type == "outcome" && (!is.null(rare_outcome) || !is.null(rare_exposure) || 
                            rare_outcome || rare_exposure)) {
    mess <- paste(mess,
                  "No rare outcome/exposure arguments are necessary for outcome misclassfication; they have been ignored.",
                  sep = "\n"
    )
  }
  
  obj <- paste(gsub("\\\"", "'", deparse(sys.call())), collapse = " ")
  class(obj) <- "bias"
  
  if (type == "outcome") {
    
    # remove any leading line breaks just for aesthetics
    if (!is.null(mess)) {
      if (substr(mess, 1, 1) == "\n") mess <- substr(mess, 2, nchar(mess))
    }
    
    if (verbose & !is.null(mess)) wrapmessage(mess)
    
    names(obj) <- "outcome misclassification"
    attr(obj, "n") <- 1
    attr(obj, "d") <- 0
    attr(obj, "message") <- mess
    
    invisible(obj)
  } else {
    
    if (type == "exposure" && is.null(rare_outcome) && is.null(rare_exposure)) {
      stop("For exposure misclassification, arguments rare_outcome and rare_exposure must be TRUE or FALSE.")
    }
    
    if (type == "exposure" && !rare_outcome) {
      stop("Exposure misclassification with non-rare outcomes not currently available for multiple bias analysis; confirm and set rare_outcome = TRUE to use this method.")
    }
    
    # only have type = exposure left
    # remove any leading line breaks just for aesthetics
    if (!is.null(mess)) {
      if (substr(mess, 1, 1) == "\n") mess <- substr(mess, 2, nchar(mess))
    }
    
    if (verbose & !is.null(mess)) wrapmessage(mess)
    
    names(obj) <- "exposure misclassification"
    attr(obj, "message") <- mess
    attr(obj, "rare_outcome") <- TRUE
    
    if (rare_exposure) {
      attr(obj, "n") <- 1
      attr(obj, "d") <- 0
      attr(obj, "rare_exposure") <- TRUE
      
      invisible(obj)
    } else {
      
      # if common exposure, this is actually an odds ratio, so is square-rooted to become a risk ratio
      attr(obj, "n") <- 2
      attr(obj, "d") <- 0
      attr(obj, "rare_exposure") <- FALSE
      
      invisible(obj)
    }
  }
}

#'@export
print.bias <- function(x, ...) {
  if (!inherits(x, "bias")) stop('Argument must be of class "bias"')
  
  attr(x, "names")
}

#' @description Create a table of the biases and their parameters
#' @noRd
get_arg_tab <- function() {
  data.frame(
    bias = c(
      "confounding", "confounding",
      "selection", "selection", "selection", "selection", "selection",
      "selection", "selection", "selection", "selection",
      "selection", "confounding and selection",
      "confounding and selection", "outcome misclassification",
      "exposure misclassification", "exposure misclassification",
      "outcome misclassification", "exposure misclassification",
      "exposure misclassification"
    ),
    order = c(1, 1, 2, 23, 2, 23, 3, 3, 3, 3, 3, 3, 1, 1, 2, 2, 2, 3, 3, 3),
    selected = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE
    ),
    rare_outcome = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
      FALSE, TRUE, TRUE
    ),
    rare_exposure = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE
    ),
    increased_risk = c(
      FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
      TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    ),
    decreased_risk = c(
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
      FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    ),
    SU = c(
      FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE,
      FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    ),
    latex = c(
      "$\\text{RR}_{AU_c}$", "$\\text{RR}_{U_cY}$", "$\\text{RR}_{U_sY \\mid A = 1}$",
      "$\\text{RR}_{SU_s \\mid A = 1}$", "$\\text{RR}_{U_sY \\mid A = 0}$",
      "$\\text{RR}_{SU_s \\mid A = 0}$", "$\\text{RR}_{U_sY^* \\mid A = 1}$",
      "$\\text{RR}_{U_sY^* \\mid A = 0}$", "$\\text{RR}_{U_sY \\mid A^* = 1}$",
      "$\\text{RR}_{SU_s \\mid A^* = 1}$", "$\\text{RR}_{U_sY \\mid A^* = 0}$",
      "$\\text{RR}_{SU_s \\mid A^* = 0}$", "$\\text{RR}_{AU_{sc}\\mid S = 1}$",
      "$\\text{RR}_{U_{sc}Y\\mid S = 1}$", "$\\text{RR}_{AY^* \\mid y}$",
      "$\\text{OR}_{YA^* \\mid a}$", "$\\text{RR}_{YA^* \\mid a}$", 
      "$\\text{RR}_{AY^* \\mid y, S = 1}$",
      "$\\text{OR}_{YA^* \\mid a, S = 1}$", "$\\text{RR}_{YA^* \\mid a, S = 1}$"
    ),
    output = c(
      "RR_AUc", "RR_UcY", "RR_UsY|A=1", "RR_SUs|A=1", "RR_UsY|A=0",
      "RR_SUs|A=0", "RR_UsY*|A=1", "RR_UsY*|A=0", "RR_UsY|A*=1", "RR_SUs|A*=1",
      "RR_UsY|A*=0", "RR_SUs|A*=1", "RR_AUsc|S", "RR_UscY|S", "RR_AY*|y",
      "OR_YA*|a", "RR_YA*|a", "RR_AY*|y,S", "OR_YA*|a,S", "RR_YA*|a,S"
    ),
    argument = c(
      "RRAUc", "RRUcY", "RRUsYA1", "RRSUsA1", "RRUsYA0",
      "RRSUsA0", "RRUsYA1", "RRUsYA0", "RRUsYA1", "RRSUsA1", "RRUsYA0",
      "RRSUsA1", "RRAUscS", "RRUscYS", "RRAYy", "ORYAa", "RRYAa", "RRAYyS",
      "ORYAaS", "RRYAaS"
    ),
    stringsAsFactors = FALSE
  )
}



#' @title Create a set of biases for a multi-bias sensitivity analysis
#'
#' @description Multiple biases ([confounding()], [selection()], and/or
#' [misclassification()]) can be assessed simultaneously after creating a
#' `multi_bias` object using this function.
#'
#' @param ... Biases ([confounding()], [selection()], and/or
#'   [misclassification()]), each possibly including arguments specifying more
#'   detail about the bias of interest. Selection and confounding should be
#'   listed in the order in which they affect the data (see [ordering of the
#'   biases](../articles/multiple-bias.html#ordering-of-the-biases))
#' @param verbose Logical. If `TRUE`, returns warnings and messages immediately.
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns a list with components whose values depend on the
#'   options chosen: `n` (the degree of the polynomial in the numerator), `d`
#'   (the degree of the polynomial in the denominator), `m` (the parameters in
#'   the bias factor), `mess` (any messages/warnings that should be printed for
#'   the user), and `bias`("misclassification").
#'
#' @keywords multi-bias
#'
#' @examples
#' biases <- multi_bias(confounding(),
#'                      selection("general"))
#'
#' # print() lists the arguments for the multi_bound() function
#' print(biases)
#'
#' # summary() provides more information
#' # with parameters in latex notation if latex = TRUE
#' summary(biases, latex = TRUE)
#'
#' # Calculate a bound
#' multi_bound(biases = biases,
#'             RRAUc = 1.5, RRUcY = 2, RRUsYA1 = 1.25,
#'             RRSUsA1 = 4, RRUsYA0 = 3, RRSUsA0 = 2)
#' @export

multi_bias <- function(..., verbose = TRUE) {
  arguments <- list(...)
  # allow passing null arguments (mostly for app)
  arguments[sapply(arguments, is.null)] <- NULL

  if (!all(sapply(arguments, inherits, what = "bias"))) {
    stop('All arguments to multi_bias() function must be of class "bias"')
  }
  
  biases <- sapply(arguments, names)
  yes_confounding <- "confounding" %in% biases
  yes_selection <- "selection" %in% biases
  yes_misclassification <- any(grepl("misclassification", biases))
  yes_both <- FALSE
  selection_rows <- confounding_rows <- misclass_rows <- NULL
  first_b <- ""
  next_b <- ""
  
  if (yes_selection && yes_misclassification) {
    if (!attr(arguments[biases == "selection"][[1]], "selected")) {
      selection_first <- grep("selection", biases) < grep("misclassification", biases)
      first_b <- ifelse(selection_first, "selection", grep("misclassification", biases, value = TRUE))
      next_b <- ifelse(selection_first, grep("misclassification", biases, value = TRUE), "selection")
    }
  }
  
  if (yes_selection && yes_confounding) {
    if (attr(arguments[biases == "selection"][[1]], "selected")) {
      next_b <- biases[!(biases == "selection" | biases == "confounding")]
      new_biases <- c("confounding and selection", next_b)
      first_b <- "confounding and selection"
      yes_confounding <- FALSE
      yes_selection <- FALSE
      yes_both <- TRUE
    } else new_biases <- biases
  } else new_biases <- biases
  
  arg_tab <- get_arg_tab()
  
  if (yes_selection && !yes_confounding && attr(arguments[biases == "selection"][[1]], "selected")) {
    arg_tab$bias[arg_tab$bias == "confounding and selection"] <- "selection"
    arg_tab[!names(arg_tab) == "bias"] <- apply(arg_tab[!names(arg_tab) == "bias"], 2, gsub, pattern = "c", replacement = "")
  }
  
  sub_tab <- arg_tab[arg_tab$bias %in% new_biases, ]
  sub_sub_tab <- sub_tab[!((sub_tab$bias == first_b & sub_tab$order == 3) |
                             (sub_tab$bias == next_b & sub_tab$order == 2)), ]
  if (nrow(sub_sub_tab) > 0) sub_tab <- sub_sub_tab
  
  if (yes_selection) {
    sel_names <- c("selected", "increased_risk", "decreased_risk", "SU")
    if (!attr(arguments[biases == "selection"][[1]], "SU")) {
      sub_tab$SU <- NULL
      sel_names <- sel_names[!sel_names == "SU"]
    }
    if (!attr(arguments[biases == "selection"][[1]], "increased_risk")) {
      sub_tab$increased_risk  <- NULL
      sel_names <- sel_names[!sel_names == "increased_risk"]
    }
    if (!attr(arguments[biases == "selection"][[1]], "decreased_risk")) {
      sub_tab$decreased_risk <- NULL
      sel_names <- sel_names[!sel_names == "decreased_risk"]
    }
    selection_rows <- merge(sub_tab, attributes(arguments[biases == "selection"][[1]]),
                            by.x = c("bias", sel_names),
                            by.y = c("names", sel_names))
    if (next_b == "selection" | yes_misclassification) { # contains both types of misclassification
      to_remove <- ifelse(first_b == "exposure misclassification", "A\\=", "A\\*\\=")
    } else to_remove <- "(A\\*)|(Y\\*)"
    remove_rows <- grep(to_remove, selection_rows$output)
    if (length(remove_rows) > 0) selection_rows <- selection_rows[-remove_rows, ]
  }
  
  if (yes_misclassification) {
    if (any(grepl("exposure", biases))) {
      misclass_rows <- merge(sub_tab, attributes(arguments[grep("misclassification", biases)][[1]]),
                             by.x = c("bias", "rare_exposure", "rare_outcome"),
                             by.y = c("names", "rare_exposure", "rare_outcome"))
    } else {
      misclass_rows <- merge(sub_tab, attributes(arguments[grep("misclassification", biases)][[1]]),
                             by.x = c("bias"),
                             by.y = c("names"))
    }
    
    s_rows <- grep("S", misclass_rows$output)
    if (length(s_rows) > 0) {
      if (next_b == "selection" || !(yes_selection || yes_both)) misclass_rows <- misclass_rows[-s_rows, ]
      else if (first_b == "selection" || yes_selection) misclass_rows <- misclass_rows[s_rows, ]
    }
  }
  
  if (yes_confounding) confounding_rows <- sub_tab[sub_tab$bias == "confounding",]
  
  if (yes_both) selection_rows <- sub_tab[sub_tab$bias == "confounding and selection",]
  
  params <- rbind(confounding_rows[, c("bias", "output", "argument", "latex")], 
                  selection_rows[, c("bias", "output", "argument", "latex")], 
                  misclass_rows[, c("bias", "output", "argument", "latex")])
  rownames(params) <- NULL
  
  biases <- arguments
  class(biases) <- "multi_bias"
  attr(biases, "n") <- Reduce(f = "+", sapply(arguments, attr, which = "n"))
  if (yes_both) attr(biases, "n") <- attr(biases, "n") - 2 # combined selection/confounding
  attr(biases, "d") <- Reduce(f = "+", sapply(arguments, attr, which = "d"))
  if (yes_both) attr(biases, "d") <- attr(biases, "d") - 1 # combined selection/confounding
  attr(biases, "message") <- Reduce(f = c, sapply(arguments, attr, which = "message"))
  attr(biases, "parameters") <- params
  
  if (verbose && !is.null(attr(biases, "message"))) {
    wrapmessage(paste(attr(biases, "message"), collapse = "\n"))
  }
  invisible(biases)
}



#' @export
summary.multi_bias <- function(object, ..., latex = FALSE) {
  if (!inherits(object, "multi_bias")) stop('Argument must be of class "multi_bias"')
  
  params <- attr(object, "parameters")
  if (!latex) params$latex <- NULL
  
  params
}

#' @export
print.multi_bias <- function(x, ...) {
  if (!inherits(x, "multi_bias")) stop('Argument must be of class "multi_bias"')
  
  params <- attr(x, "parameters")
  
  wrapmessage(c("The following arguments can be copied and pasted into the multi_bound() function:",
                paste(params$argument, collapse = " = , "), " ="))
}