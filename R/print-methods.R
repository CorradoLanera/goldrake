#' Print goldrakes
#'
#' \code{print} prints its argument and returns it invisibly
#' (via invisible(x))
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#'
#' @return invisible(x)
#' @name print
NULL

#' @describeIn goldrake Method to print \code{\link{goldrake}}s
#' @inheritParams print
#' @return invisible \code{x}
#' @export
#'
#' @examples
#' x <- goldrake(mtcars)
print.goldrake <- function(x, ...) {

    done <- get_used_data(x)
    original <- get_original_data(x)

    golds <- get_gold_classes(x)
    if (!length(golds)) {
        golds <- crayon::red("none")
    }

    revs <- get_reviewers(x)
    if (!length(revs)) {
        revs <- crayon::red("none")
    }

    balances <- get_balanced_variables(x)
    if (!length(balances)) {
        balances <- crayon::red("none")
    }


    ui_line("goldrake classification object")
    ui_line("")
    ui_todo("{ui_field(nrow(original))} cases and {ui_field(ncol(original))} variables (balanced by {ui_field(balances)})")
    ui_todo("Classes: {ui_field(golds)}.")
    ui_line("")
    ui_done("Data classified: {ui_field(nrow(done))} (by everyone) -- {ui_field(nrow(done))} (by someone)")
    ui_fail("Data left to classify: {ui_field(nrow(original) - nrow(done))} (by someone) -- {ui_field(nrow(original) - nrow(done))} (by everyone)")
    ui_line("")
    ui_todo("Reviewers: {ui_field(revs)}.")


    invisible(x)

}
