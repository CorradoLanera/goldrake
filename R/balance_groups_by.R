#' Set balance classes
#'
#' @param x a \code{\link{goldrake}} object
#' @param ... variables to balance by
#' @param force (lgl, default = TRUE) if TRUE, overwrite possible
#'        balancing variables already set.
#'
#' @return invisibly, the updated version of x
#' @export
#'
#' @examples
#' goldrake(mtcars) %>%
#'     balance_groups_by(vs)
#'
#' goldrake(mtcars) %>%
#'     balance_groups_by(vs, cyl)
#'
#' goldrake(mtcars) %>%
#'     balance_groups_by(vs, cyl)
#'
#' goldrake(mtcars) %>%
#'     balance_groups_by(vs) %>%
#'     balance_groups_by(vs, cyl, force = TRUE)
#'
#' \dontrun{
#'     # an error
#'     goldrake(mtcars) %>%
#'         balance_groups_by(foo)
#'
##'    goldrake(mtcars) %>%
#'         balance_groups_by(vs) %>%
#'         balance_groups_by(vs, cyl)
#' }
#'
balance_groups_by <- function(x, ...) {
    UseMethod("balance_groups_by")
}

#' @rdname balance_groups_by
#' @export
balance_groups_by.default <- function(x, ...) {

    ui_stop(paste(
        "The {ui_field('x')} input in {ui_code('balance_groups_by')}",
        "must be inherited from the {ui_value('goldrake')} class.\n",
        "It is of class {ui_value(class(x))}.\n",
        "Please, provide an object of class {ui_value('goldrake')}."
    ))

}

#' @rdname balance_groups_by
#' @export
balance_groups_by.goldrake <- function(x, ..., force = FALSE) {

    dots <- names(enquos(..., .named = TRUE))

    var_names <- get_original_data(x) %>% names()

    missing_vars <- dots[!(dots %in% var_names)]

    if (length(missing_vars)) {ui_stop("arghhh")}

    balance <- get_balanced_variables(x)

    if (rlang::has_length(balance)) {
        ui_warn(paste(
            "Balancing variables are already present.\n",
            "They are: {ui_value(balance)}."
        ))

        if (!force) {
            ui_fail("Balancing variables have not been changed.")
            return(invisible(x))
        }

        ui_done("Balancing variables have been changed.")
    }



    attr(x, "balanced_variables") <- dots
    if (!force) ui_done("Balancing variables setted")
    ui_done("New balancing variables: {ui_value(dots)}")
    invisible(x)
}
