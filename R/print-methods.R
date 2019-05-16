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

    done <- get_done(x)
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
    ui_done("{ui_field(nrow(original))} cases and {ui_field(ncol(original))} variables (balanced by {ui_field(balances)})")
    ui_done("Classes: {ui_field(golds)}.")
    ui_line("")
    ui_done("Data classified: {ui_field(sum(complete.cases(done)))} (by everyone) -- {ui_field(nrow(done))} (by someone)")
    ui_todo("Data left to classify: {ui_field(nrow(get_used_data(x)) - sum(complete.cases(done)))} (by someone) -- {ui_field(nrow(get_used_data(x)) - nrow(done))} (by everyone)")
    ui_line("")
    ui_done("Reviewers: {ui_field(revs)}.")


    invisible(x)

}
