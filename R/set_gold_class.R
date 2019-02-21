#' Set gold classes
#'
#' @param x a \code{\link{goldrake}} object
#' @param classes (chr, default c("0", "1")) Classes admitted for the
#'        classification.
#'
#' @return a \code{\link{goldrake}} object with updated classes
#' @export
#'
#' @examples
#' goldrake(mtcars) %>%
#'     set_gold_classes()
#'
#' goldrake(mtcars) %>%
#'     set_gold_classes(c("good", "bad", "so and so"))
#'
#' goldrake(mtcars) %>%
#'     set_gold_classes(c("good", "bad", "so and so")) %>%
#'     set_gold_classes(c("0", "1"))
#'
#' \dontrun{
#'     # an error
#'     set_gold_classes(1)
#'
#'     # another error
#'     goldrake(mtcars) %>%
#'         set_gold_classes(1)
#' }
#'
set_gold_classes <- function(x, classes) {
    UseMethod("set_gold_classes")
}

#' @rdname set_gold_class
set_gold_classes.default <- function(x, classes) {

    ui_stop(paste(
        "The {ui_field('x')} input in {ui_code('set_gold_classes')}",
        "must be inherited from the {ui_value('goldrake')} class.\n",
        "It is of class {ui_value(class(x))}.\n",
        "Please, provide an object of class {ui_value('goldrake')}."
    ))

}

#' @rdname set_gold_class
set_gold_classes.goldrake <- function(x, classes = c("0", "1")) {

    if (!is.character(classes)) {
        ui_stop(paste0(
            "The {ui_field('classes')} input in",
            "{ui_code('set_gold_classes')} must be inherited from the",
            "{ui_value('character')} class.\n",
            "It is of class {ui_value(class(classes))}.\n",
            "Please, provide an object of class",
            "{ui_value('character')}."
        ))
    }

    actual_classes <- gold_classes(x)
    has_classes    <- length(actual_classes) != 0L

    if (has_classes) {
        ui_warn(paste(
            "Gold classes already present.\n",
            "They are: {ui_value(actual_classes)}."
        ))
        ui_fail("Classes have not been changed.")
        return(x)
    }

    attr(x, "gold_classes") <- classes
    x

}
