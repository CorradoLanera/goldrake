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
#' \dontrun{
#'     # throw an error
#'     goldrake(1)
#' }
goldrake <- function(original_data) {

    if (!is.data.frame(original_data)) {
        ui_stop(paste(
            "The {ui_field('original_data')} input in",
            "{ui_code('goldrake')} must be inherited from the",
            "{ui_value('data.frame')} class.\n",
            "It is of class {ui_value(class(original_data))}.\n",
            "Please, provide an object of class",
            "{ui_value('data.frame')}."
        ))
    }

    structure(
        .Data = goldrake_list(original_data),

        reviewer_names     = character(),
        gold_classes       = character(),
        balanced_variables = character(),
        max_sampled        = c(
            overall  = integer(),
            by_group = integer()
        ),

        class = "goldrake"
    )
}


# goldrake.goldrake <- function(x) x
