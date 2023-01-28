#' @title Accessor function for the 'mdata' slot of an MArray object
#' @param object An object of class MArray
#' @rdname mdata
#' @export
setGeneric("mdata", function(object) standardGeneric("mdata"))

#' @title Accessor function for the 'pmeta' slot of an MArray object
#' @param object An object of class MArray
#' @rdname pmeta
#' @export
setGeneric("pmeta", function(object) standardGeneric("pmeta"))

#' @title Accessor function for the 'fmeta' slot of an MArray object
#' @param object An object of class MArray
#' @rdname fmeta
#' @export
setGeneric("fmeta", function(object) standardGeneric("fmeta"))

#' @title Replacement method for the 'mdata' slot of an MArray object
#' @param object An object of class MArray
#' @param value Replacement item
#' @rdname mdata
#' @export
setGeneric("mdata<-", function(object, value) standardGeneric("mdata<-"))

#' @title Replacement method for the 'fmeta' slot of an MArray object
#' @param object An object of class MArray
#' @param value Replacement item
#' @rdname fmeta
#' @export
setGeneric("fmeta<-", function(object, value) standardGeneric("fmeta<-"))

#' @title Replacement method for the 'pmeta' slot of an MArray object
#' @param object An object of class MArray
#' @param value Replacement item
#' @rdname pmeta
#' @export
setGeneric("pmeta<-", function(object, value) standardGeneric("pmeta<-"))
