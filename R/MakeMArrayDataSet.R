#' @title Simulate a Micro array data set with accompanying metadata
#' @description Simulate Micro array data by randomly drawing values from a normal distribution centered at 10 with a standard deviation of 5
#' @param n_samples The number of samples to simulate (must be a positive, even integer value that is no greater than '26')
#' @param n_features The number of genes to simulate (must be a positive, even integer value)
#' @param with_seed A numeric input to set the seed of the session (defaults to '8675309')
#'
#' @return A list of 1 matrix ('marray' - simulated Micro aeray data), and 2 data frames (1 for sample metadata -- 'pmeta' -- and another for gene metadata -- 'fmeta')
#' @export
#' @importFrom stats rnorm
#'
#' @examples
#' MakeMArrayDataSet(n_samples = 16L, n_features = 12L, with_seed = 8675)
MakeMArrayDataSet <- function(n_samples = 10L, n_features = 6L, with_seed = 8675309) {

  stopifnot("Inputs must be numeric values" = (is.numeric(n_samples) & is.numeric(n_features)))
  stopifnot("Inputs must be positive values" = ((n_samples > 0) & (n_features > 0)))
  stopifnot("Inputs must be integer values" = (is.integer(n_samples) & is.integer(n_features)))
  stopifnot("'n_samples' cannot be larger than 26" = (n_samples <= 26))
  stopifnot("Inputs must be divisible by 2" = ((n_samples %% 2 == 0) & (n_features %% 2 == 0)))
  stopifnot("'with_seed' must be null or numeric type"=(is.null(with_seed) | is.numeric(with_seed)))

  if(!is.null(with_seed)){set.seed(with_seed)}

  #test

  n <- n_features
  m <- n_samples

  marray <- matrix(rnorm(n * m, 10, 5), ncol = m)
  pmeta <- data.frame(sampleId = 1:m,
                      condition = rep(c("WT", "MUT"), each = m/2))
  rownames(pmeta) <- colnames(marray) <- LETTERS[1:m]
  fmeta <- data.frame(geneId = 1:n,
                      pathway = sample(LETTERS, n, replace = TRUE))
  rownames(fmeta) <-
    rownames(marray) <- paste0("probe", 1:n)

  maexp <- list(marray = marray,
                fmeta = fmeta,
                pmeta = pmeta)

  rm(marray, fmeta, pmeta) ## clean up

  return(maexp)

}
