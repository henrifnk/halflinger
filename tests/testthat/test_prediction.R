####  test prediction  #########################################################
# use <generate_train_data> function
# make depth::depth accept matrices containing points u from test as rows:
context("predict.halfspaces")

hsdepth <- function(test, train) {
  test_data <- test
  apply(test_data, 1, depth::depth, x = train)
}
# wrap train and test steps into a single function:
my_hsdepth <- function(test, train, ...) {
  predict(
    object = train_depth(train, ...),
    data = test,
    metric = "depth")
}

test_that("compare ranks and check correlation with depth pkg", {
  data_list <- generate_train_data()
  for (train in data_list) {
    approx_depth <- my_hsdepth(test = data_list$grid[[1]], train = train[[1]],
                               n_halfspace = 1e4, scope = 1, seed = 23)
    exact_depth <- hsdepth(test = data_list$grid[[1]], train = train[[1]])
    expect_true(
      cor(approx_depth,
         exact_depth) > 0.99
    )
    expect_equivalent(
      rank(approx_depth),
      rank(exact_depth),
      tol = nrow(data_list$grid[[1]])/20
    )
  }
})

test_that("compare ranks of scaled values with depth pkg", {
  data_list <- generate_train_data()
  for (train in data_list) {
    approx_depth <- my_hsdepth(test = data_list$grid[[1]], train = train[[1]],
                               n_halfspace = 1e4, scope = 1, seed = 23, scale = TRUE)
    exact_depth <- hsdepth(test = data_list$grid[[1]], train = train[[1]])
    expect_equivalent(
      rank(approx_depth),
      rank(exact_depth),
      tol = nrow(data_list$grid[[1]])/20
    )
  }
})

test_that("check scaled prediction", {
  data_list <- generate_train_data(scale = TRUE)
  predictions <- lapply(data_list, function(x) {
    predict(do.call(train_depth, x), data_list$grid[[1]])
  })
  for (prediction in predictions) {
    expect_lt(sd(prediction), 1)
    expect_equal(mean(prediction), 0, tol = 1)
  }
})
