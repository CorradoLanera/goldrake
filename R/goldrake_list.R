#' goldrake list component
#'
#' This function create the list-component of a goldrake object
#'
#' @param .x a data.frame.
#'
#' @return a list with three components: \code{used_data} (an empty
#'         [tibble][tibble::tibble-package]), \code{reviewers} (an empty
#'         list), and \code{original_data} (.x as the original data).
#'
goldrake_list <- function(.x) {
    if (!is.data.frame(.x)) stop(
        "You have to put in input a data.frame like object.\n",
        "  The object you provide is of class: ", class(.x), ".\n\n",
        "  Please provide a data.frame like object."
    )

    list(
        used_data     = tibble(),
        reviewers     = tibble(
            id = integer(),
            code = character(),
            name = character(),
            surname = character(),
            inspector = logical()
        ),
        original_data = .x
    )
}
