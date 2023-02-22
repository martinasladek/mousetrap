#' Get Sniffy data for the take-away paper
#'
#' @param cand_no Candidate number
#'
#' @return A tibble of Sniffy data.
#' @export
#'
#' @examples
#' get_data(123456)
#'
#'

get_data <- function(cand_no){

  tryCatch(
    {
      make_sniffy_data(cand_no)
    },
    error = function(cond){
      message("You need to specify your candidate number to run the function. The candidate number can found on Sussex Direct or on your student card labelled as CandNo (not to be confused with the registration number!)")
    }
  )
}
