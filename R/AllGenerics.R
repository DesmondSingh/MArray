#' @title Accessor function for the 'marray' slot of an MArray object
#' @rdname marray
#' @export
setGeneric("marray", function(object) standardGeneric("marray"))

#' @title Accessor function for the 'pmeta' slot of an MArray object
#' @rdname pmeta
#' @export
setGeneric("pmeta", function(object) standardGeneric("pmeta"))

#' @title Accessor function for the 'fmeta' slot of an MArray object
#' @rdname fmeta
#' @export
setGeneric("fmeta", function(object) standardGeneric("fmeta"))

#' @title Replacement method for the 'marray' slot of an MArray object
#' @rdname marray
#' @export
setGeneric("marray<-", function(object, value) standardGeneric("marray<-"))

#' @title Replacement method for the 'fmeta' slot of an MArray object
#' @rdname fmeta
#' @export
setGeneric("fmeta<-", function(object, value) standardGeneric("fmeta<-"))

#' @title Replacement method for the 'pmeta' slot of an MArray object
#' @rdname pmeta
#' @export
setGeneric("pmeta<-", function(object, value) standardGeneric("pmeta<-"))
