#' @rdname marray
#' @export
setGeneric("marray", function(object, ...) standardGeneric("marray"))

#' @rdname pmeta
#' @export
setGeneric("pmeta", function(object) standardGeneric("pmeta"))

#' @rdname fmeta
#' @export
setGeneric("fmeta", function(object) standardGeneric("fmeta"))

#' @rdname marray<-
#' @export
setGeneric("marray<-",
           function(object, value) standardGeneric("marray<-"))

#' @rdname fmeta<-
#' @export
setGeneric("fmeta<-", function(object, value) standardGeneric("fmeta<-"))

#' @rdname pmeta<-
#' @export
setGeneric("pmeta<-", function(object, value) standardGeneric("pmeta<-"))
