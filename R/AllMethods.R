#' @title Accessor function for the 'mdata' slot of an MArray object
#' @param object An object of class MArray
#' @return A matrix of Micro array data
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' mdata(ma_obj)
setMethod("mdata", "MArray", function(object){object@mdata})

#' @title Accessor function for the 'pmeta' slot of an MArray object
#' @param object An object of class MArray
#' @return A dataframe of sample meta data
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' pmeta(ma_obj)
setMethod("pmeta", "MArray", function(object) object@pmeta)

#' @title Accessor function for the 'fmeta' slot of an MArray object
#' @param object An object of class MArray
#' @return A dataframe of feature meta data
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' fmeta(ma_obj)
setMethod("fmeta", "MArray", function(object) object@fmeta)

#' @title Replacement method for the 'mdata' slot of an MArray object
#' @param object An object of class MArray
#' @param value An item you wish to assign to the 'mdata' slot
#' @return An MArray object with its 'mdata' slot updated
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' mdata(ma_obj) <- ma$mdata + 100
setMethod("mdata<-", "MArray",
          function(object, value) {
            object@mdata <- value
            if (validObject(object))
              return(object)
          })


#' @title Replacement method for the 'fmeta' slot of an MArray object
#' @param object An object of class MArray
#' @param value An item you wish to assign to the 'fmeta' slot
#' @return An MArray object with its 'fmeta' slot updated
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma2 <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 309)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' fmeta(ma_obj) <- ma2$fmeta
setMethod("fmeta<-", "MArray",
          function(object, value) {
            object@fmeta <- value
            if (validObject(object))
              return(object)
          })

#' @title Replacement method for the 'pmeta' slot of an MArray object
#' @param object An object of class MArray
#' @param value An item you wish to assign to the 'pmeta' slot
#' @return An MArray object with its 'pmeta' slot updated
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma2 <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 309)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' pmeta(ma_obj) <- ma2$pmeta
setMethod("pmeta<-", "MArray",
          function(object, value) {
            object@pmeta <- value
            if (validObject(object))
              return(object)
          })


#' @title Show method for an MArray object
#' @param object An object of class MArray
#' @return Text describing the dimensions of an MArray object
#' @export
#' @importFrom methods setMethod new validObject show
#' @examples
#' toy <- MArray::MakeMArrayDataSet()
#' toy_ma <-MArray(mdata = toy$mdata, fmeta = toy$fmeta, pmeta = toy$pmeta)
#' show(toy_ma)
setMethod("show",
          signature = "MArray",
          definition = function(object) {
            cat("An object of class ", class(object), "\n", sep = "")
            cat(" ", nrow(object@mdata), " features by ",
                ncol(object@mdata), " samples.\n", sep = "")
            invisible(NULL)
          })


#' @title Subsetting MArray objects
#' @param x An object of class "MArray"
#' @param i A row index value
#' @param j A column index value
#' @param drop Set to 'missing' to ignore irrelevant arguments
#' @return A subset of an original "MArray" object
#' @export
#' @examples
#' ma <- MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
#' ma_obj <- MArray(mdata = ma$mdata, fmeta = ma$fmeta, pmeta = ma$pmeta)
#' sub_ma_obj <- ma_obj[1:3,1:4]
#' show(sub_ma_obj)

setMethod("[", "MArray",
          function(x,i,j,drop="missing") {
            .mdata <- x@mdata[i, j]
            .pmeta <- x@pmeta[j, ]
            .fmeta <- x@fmeta[i, ]
            MArray(mdata = .mdata,
                   fmeta = .fmeta,
                   pmeta = .pmeta)
          })
