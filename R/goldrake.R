#' Goldrakes
#'
#' The function \code{\link{goldrake}} creates goldrake objects:
#' collection of data, classes defined by expert reviewers, and
#' sampling characteristics used to create a gold standard data set
#' for classification tasks.
#'
#' @param original_data a data frame like object with the original
#'        dataset.
#'
#' @return a [goldrake][goldrake::goldrake-package] object.
#' @export
#'
#' @examples
#' goldrake(mtcars)
goldrake <- function(original_data) {

    structure(
        .Data = goldrake_list(original_data),

        reviewer_names     = character(),
        gold_classes       = factor(),
        balanced_variables = character(),
        max_sampled        = c(
            overall  = integer(),
            by_group = integer()
        ),

        class = "goldrake"
    )
}
