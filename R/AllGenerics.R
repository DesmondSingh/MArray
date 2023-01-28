#' @title Getter for marray slot of MArray object
#' @rdname marray
#' @export
setGeneric("marray", function(object, ...) standardGeneric("marray"))

#' @title Getter for pmeta slot of MArray object
#' @rdname pmeta
#' @export
setGeneric("pmeta", function(object) standardGeneric("pmeta"))

#' @title Getter for fmeta slot of MArray object
#' @rdname fmeta
#' @export
setGeneric("fmeta", function(object) standardGeneric("fmeta"))

#' @title Setter for marray slot of MArray object
#' @rdname marray<-
#' @export
setGeneric("marray<-",
           function(object, value) standardGeneric("marray<-"))

#' @title Setter for fmeta slot of MArray object
#' @rdname fmeta<-
#' @export
setGeneric("fmeta<-", function(object, value) standardGeneric("fmeta<-"))

#' @title Setter for pmeta slot of MArray object
#' @rdname pmeta<-
#' @export
setGeneric("pmeta<-", function(object, value) standardGeneric("pmeta<-"))
