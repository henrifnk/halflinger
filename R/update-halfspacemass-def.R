#' @title update.halfspaces
#'
#' @description Update-Method to [train_depth()] for halfspaces objects.
#'     Implementation train algorithm of Chen et al. for adding and changing
#'     `data` and amount of `halfspaces`.
#'
#' @details Functionality after input check can be summed up in 3 main steps:
#'    ## 1st Step [add_data()] & [prepare_data()]
#'    check the input on type and scale & prepare data for calculation
#'    ## 2nd Step [update_halfspaces()]
#'    update Halfspaces, already calculated in the input `object`
#'    ## 3rd Step [add_halfspaces()]
#'    get new Halfspaces
#'
#' @param object a halfspaces object containing a list arbitrary length of
#'    halfspace objects.
#' @param add logical, indicating wether `data` should be added to `train_data`
#'     from `object`
#' @inheritParams train_depth
#' @param ... ellipsis
#' @result an updated halfspaces object, reclaculated on an updated dataset
#'     and/or on a new amount of `n_halfspaces`.
#'
#' @references
#'     Chen, B., Ting, K.M., Washio, T. et al.,
#'     Mach Learn (2015): 100(2):677--699
#'     Half-space mass a maximally robust and efficient data depth method
#'     \url{https://doi.org/10.1007/s10994-015-5524-x}
#' @seealso [train_depth()], [autoplot.halfspaces()], [predict.halfspaces()]
#'
#' @export
update.halfspaces <- function(object, data = attr(object, "train_data"),
                              n_halfspace = length(object), add = FALSE, ...) {

  checkmate::assert_true(identical(validate_halfspaces(object), object))
  checkmate::assert_true(
    identical(colnames(data), colnames(attr(object, "train_data")))
  )
  checkmate::assert_integerish(n_halfspace, len = 1, lower = 1)
  checkmate::assert_logical(add, len = 1)

  complete_data <- add_data(attr(object, "train_data"),
                            data = data, add = add)
  input <- attributes(object)[!names(attributes(object))
                              %in% c("train_data", "class")]
  if (!is.null(input[["seed"]])) set.seed(as.integer(input[["seed"]]))

  updated_hs <- update_halfspaces(object, complete_data, n_halfspace, input)
  new_hs <- add_halfspaces(complete_data, n_halfspace - length(object), input)

  halfspaces(c(updated_hs, new_hs), train_data = complete_data,
             subsample = input[["subsample"]], scope = input[["scope"]],
             seed = input[["seed"]], scale = input[["scale"]]
             )
  }

# utilities --------------------------------------------------------------------

#' @title Add data
#'
#' @description This function takes the observations from train data and the
#' observations from input data and returns their union amount if add is set to
#' TRUE; in addtion add_data formats the `data`.
#'
#' @param train_data the dataset a concerning `object` was trained on
#' @inheritParams update.halfspaces
#' @return A data set with all containing required unique observations to
#' [train_depth()]
add_data <- function(train_data, data, add){

  if (!add) return(data)
  unique(rbind(train_data, data))
}

#' @title Update Halfspaces
#'
#' @description This function updates the already drawn `halfspaces` of the
#'     input `object`.
#'
#' @details Functionality splits in two scenarios:
#'    ## Scenario for an unchanged dataset
#'    if dataset remains unchanged, halfspace objects are identical, too
#'    drawn halfspaces are a subset of `object`
#'    ## Scenario for a changed dataset
#'    if dataset changes, new randomly drawn subsamples are needed which provide
#'    a new base for splitpoint criteria and mass_above, therefore whole
#'    <get_halfspaces> needs to be recalled on each direction of the dataset
#'
#' @inheritParams update.halfspaces
#' @param complete_data A data set with all containing required unique
#' observations to [train_depth()]
#' @param input the parameter set an object was trained on
#' @importFrom utils tail
#' @return A data set that is ready to update [train_depth()] on
update_halfspaces <- function(object, complete_data, n_halfspace, input) {

  length_hs <-  min(length(object), n_halfspace)

  if (identical(complete_data, attr(object, "train_data"))) {
    warning("Output Object contains a subset of the Input Object")
    return(tail(object, length_hs))
  }

  warning("Due to new modified Data, new splitpoints are drawn randomly")
  train_depth(complete_data, n_halfspace = length_hs,
             subsample = input[["subsample"]], scope = input[["scope"]],
             seed = input[["seed"]], scale = input[["scale"]])
}

#' @title Add Halfspaces
#'
#' @description This function calculates `n_halfspaces` new halfspaces from
#' `complete_data`
#'
#' @inheritParams update_halfspaces
#' @result a halfspace object of <n_halfspace> halfspaces
#'
#' @seealso [train_depth()]
add_halfspaces <- function(complete_data, n_halfspace, input) {

  checkmate::assertIntegerish(n_halfspace, any.missing = FALSE, len = 1)

  if (n_halfspace < 1) return(list())
  do.call("train_depth", c(list(complete_data, n_halfspace), input))
}
