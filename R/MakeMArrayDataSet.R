MakeMArrayDataSet <- function(n_samples = 10L, n_features = 6L, with_seed = 8675309) {

  stopifnot("Inputs must be numeric values" = (is.numeric(n_samples) & is.numeric(n_features)))
  stopifnot("Inputs must be positive values" = ((n_samples > 0) & (n_features > 0)))
  stopifnot("Inputs must be integer values" = (is.integer(n_samples) & is.integer(n_features)))
  stopifnot("Inputs must be divisible by 2" = ((n_samples %% 2 == 0) & (n_features %% 2 == 0)))
  stopifnot("'n_samples' cannot be larger than 26" = (n_samples <= 26))
  stopifnot("'with_seed' must be null or numeric type"=(is.null(with_seed) | is.numeric(with_seed)))

  if(!is.null(with_seed)){set.seed(with_seed)}


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
