#' @rdname MArray
#' @export

MArray <- setClass("MArray",

                   slots = c(marray = "matrix",
                             fmeta = "data.frame",
                             pmeta = "data.frame"))


setValidity("MArray", function(object){
  msg <- NULL
  valid <- TRUE
  if (nrow(marray(object)) != nrow(fmeta(object))) {
    valid <- FALSE
    msg <- c(msg,
             "Number of data and feature meta-data rows must be identical.")
  }
  if (ncol(marray(object)) != nrow(pmeta(object))) {
    valid <- FALSE
    msg <- c(msg,
             "Number of data rows and sample meta-data columns must be identical.")
  }
  if (!identical(rownames(marray(object)), rownames(fmeta(object)))) {
    valid <- FALSE
    msg <- c(msg,
             "Data and feature meta-data row names must be identical.")
  }
  if (!identical(colnames(marray(object)), rownames(pmeta(object)))) {
    valid <- FALSE
    msg <- c(msg,
             "Data row names and sample meta-data columns names must be identical.")
  }
  if (valid) TRUE else msg


})

