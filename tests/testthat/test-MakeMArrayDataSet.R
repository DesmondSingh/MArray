test_that("MakeMArray works when it should (and doesn't when it shouldn't)", {

  expect_no_error(MakeMArrayDataSet())
  expect_no_error(MakeMArrayDataSet(n_samples = 10L,
                                    n_features = 6L,
                                    with_seed = NULL))

  expect_no_error(MakeMArrayDataSet(n_samples = 10L,
                                    n_features = 6L,
                                    with_seed = 12453))

  expect_equal(length(MakeMArrayDataSet()), 3)
  expect_equal(is.list(MakeMArrayDataSet()), T)

  ma <- MakeMArrayDataSet()
  expect_equal(is.matrix(ma[["marray"]]),T)
  expect_equal(is.data.frame(ma[["fmeta"]]),T)
  expect_equal(is.data.frame(ma[["pmeta"]]),T)
  expect_equal(nrow(ma[["marray"]]), nrow(ma[["fmeta"]]))
  expect_equal(ncol(ma[["marray"]]), nrow(ma[["pmeta"]]))


  expect_error(MakeMArrayDataSet(n_samples = 10 - 1))
  expect_error(MakeMArrayDataSet(n_features = 6 - 1))

  expect_error(MakeMArrayDataSet(n_samples = 10))
  expect_error(MakeMArrayDataSet(n_features = 6))

  expect_error(MakeMArrayDataSet(n_samples = -10))
  expect_error(MakeMArrayDataSet(n_features = -6))

  expect_error(MakeMArrayDataSet(n_samples = 27))

  expect_error(MakeMArrayDataSet(n_samples = "toy"))
  expect_error(MakeMArrayDataSet(n_features = "toy"))
  expect_error(MakeMArrayDataSet(with_seed = "toy"))


})
