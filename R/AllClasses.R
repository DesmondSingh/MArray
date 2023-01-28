#' @title Constructor for making an instance of an 'MArray' object
#' @param mdata matrix
#' @param pmeta dataframe
#' @param fmeta dataframe
#' @return Object of class "MArray"
#' @aliases MArray-Class
#' @docType class
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' show(ma_obj)
#' @rdname MArray
#' @importFrom methods setClass
#' @export

MArray <- setClass("MArray",
                   slots = c(mdata = "matrix",
                             fmeta = "data.frame",
                             pmeta = "data.frame"))

setValidity("MArray", function(object) {
  msg <- NULL
  valid <- TRUE
  if (nrow(mdata(object)) != nrow(fmeta(object))) {
    valid <- FALSE
    msg <- c(msg,
             "Number of data and feature meta-data rows must be identical.")
  }
  if (ncol(mdata(object)) != nrow(pmeta(object))) {
    valid <- FALSE
    msg <- c(msg,
             "Number of data rows and sample meta-data columns must be identical.")
  }
  if (!identical(rownames(mdata(object)), rownames(fmeta(object)))) {
    valid <- FALSE
    msg <- c(msg,
             "Data and feature meta-data row names must be identical.")
  }
  if (!identical(colnames(mdata(object)), rownames(pmeta(object)))) {
    valid <- FALSE
    msg <- c(msg,
             "Data row names and sample meta-data columns names must be identical.")
  }
  if (valid) TRUE else msg
})





