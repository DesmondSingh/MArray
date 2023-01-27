#' Constructor for making an instance of an 'MArray' object
#'
#' @param marray matrix
#' @param pmeta dataframe
#' @param fmeta dataframe
#'
#' @return Object of clas "MArray"
#' @export
#' @importFrom methods setClass
#'
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(marray = ma$marray, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' show(ma_obj)

MArray <- setClass("MArray",

                   slots = c(marray = "matrix",
                             fmeta = "data.frame",
                             pmeta = "data.frame"),

                   prototype = list(marray=matrix(0, dimnames=list('gene A','sample 1')),
                                    fmeta=data.frame('Yes', row.names = 'gene A'),
                                    pmeta=data.frame('M', row.names = 'sample 1')),

                   validity = function(object){
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
                   }

)


