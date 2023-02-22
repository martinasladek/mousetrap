#' Make random ID codes
#'
#' @param n Number of characters
#'
#' @return a character verctor of IDs
#'
#' @examples
#'  \dontrun{
#'  make_ids(10)
#' }
make_ids <- function(n) {
  paste(sample(c(0:9, LETTERS), n), collapse = "")
}


#' Create or replace ID codes
#'
#' @param data A datset
#'
#' @return a tibble or a dataframe with fixed IDs
#'
#' @examples
#'  \dontrun{
#'  fix_ids(sniffy_data)
#' }

fix_ids <- function(data){
  data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      id = make_ids(6)
    ) |>
    dplyr::ungroup()
  return(data)
}

#' Create random Sniffy-style data
#'
#' @param cand_no A candidate number
#' @param n Number of observations to create per condition
#' @param messy Should the output be "messed up" with bad values? Default is TRUE.
#'
#' @import faux dplyr tibble tidyr tidyselect
#'
#' @return A tibble of Sniffy data
#'
#' @examples
#'  \dontrun{
#'  make_sniffy_data(12345)
#' }
#'
make_sniffy_data <- function(cand_no, n = 10, messy = TRUE) {

  set.seed(cand_no)

  sniffy <- sniffy |>
    dplyr::mutate(condition = factor(condition))

  sniffy_data <- sniffy |>
    dplyr::select(-rat_id) |>
    dplyr::filter(condition != 0) |>
    faux::sim_df(n = n, between = "condition") |>
    dplyr::mutate(
      across(tidyselect::where(is.numeric), ~round(abs(.x))) ## Make whole numbers and drop negatives
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        condition = factor(0),
        reward = sample(40:65, n, replace = TRUE),
        responses = reward *6
      )
    ) |>
    dplyr::arrange(condition) |>
    fix_ids()

  ## Mess up the data from here, if messy = TRUE

  if(messy){

    too_many <- sample(101:200, size = sample(2:5, 1))
    too_few <- rep(0, times = sample(2:5, 1))

    missing <- rep(NA, times = sample(2:5, 1))

    ## Take a random sample of rows and add messy values
    reward_messy <- sniffy_data |>
      dplyr::slice_sample(n = length(c(too_many, too_few))) |>
      dplyr::mutate(
        reward = c(too_many, too_few)
      ) |>
      fix_ids()

    missing_messy <- sniffy_data |>
      dplyr::slice_sample(n = length(missing)) |>
      dplyr::mutate(
        responses = missing
      ) |>
      fix_ids()

    sniffy_data <- rbind.data.frame(sniffy_data, reward_messy, missing_messy)|>
      dplyr::ungroup() |>
      dplyr::arrange(condition)

    ## Mess up the names
    id_names <- c("id", "ID", "i.d.", "id_num", "id_no")
    condition_names <- c("condition", "CONDITION", "cond", "Condition", "COND")
    response_names <- c("response", "RESPONSE", "resp", "Response", "RESP")
    reward_names <- c("reward", "REWARD", "rew", "Reward", "REW")

    names_grid <- tidyr::expand_grid(id_names, condition_names, response_names, reward_names)

    messed_up_names <- unlist(names_grid[sample(1:nrow(names_grid), size = 1), ])

    names(sniffy_data) <- messed_up_names
  }

  return(sniffy_data)
}
