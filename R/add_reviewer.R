#' Add reviewer
#'
#' @param x a \code{\link{goldrake}} object
#' @param name (chr) Reviewer's name
#' @param surname  (chr, default = NULL, optional) Reviewer's surname
#' @param inspector (lgl, default = FALSE) Is a reviewer a reviever's
#'        reviewers?
#' @param seed (num, default = 1) seed for the case sampling
#'
#' @return invisibly, the updated version of x
#' @export
#'
#' @examples
#' with_reviewer <- goldrake(mtcars) %>%
#'     set_gold_classes() %>%
#'     add_reviewer("Reviewer 1")
#'
#' with_reviewer
add_reviewer <- function(x,
    name, surname = NULL, inspector = FALSE, seed = 1) {
    UseMethod("add_reviewer")
}

#' @rdname add_reviewer
#' @export
add_reviewer.default <- function(
    x, name, surname = NULL, inspector = FALSE, seed = 1
) {
    ui_stop(paste(
        "The {ui_field('x')} input in {ui_code('add_reviewer')}",
        "must be inherited from the {ui_value('goldrake')} class.\n",
        "It is of class {ui_value(class(x))}.\n",
        "Please, provide an object of class {ui_value('goldrake')}."
    ))
}

#' @rdname add_reviewer
#' @export
add_reviewer.goldrake <- function(
    x, name, surname = NULL, inspector = FALSE, seed = 1
) {
    if (length(name) != 1) ui_stop(paste(
        "You must {ui_field('add_reviewer')} for a single reviewer at time.\n",
        "Reviewer's name must be a single string."
    ))

    if (!length(get_gold_classes(x))) ui_stop(paste(
        "{ui_code('x')} has no gold classes set up.\n",
        "Please, set them by {ui_code('set_gold_classes()')} before to {ui_code('add_reviewer()')}"
    ))

    rev_id <- length(get_reviewers(x)) + 1L
    rev_code <- paste0("rev_", rev_id)
    reviewer <- tibble::tibble(
        id   = rev_id,
        code = rev_code,
        name = name,
        surname = surname %||% NA_character_,
        inspector      = inspector %||% NA_character_
    )

    full_name <- stringr::str_c(name, surname, sep = ", ")

    old_reviewers <- get_reviewers(x)
    if (any(full_name == old_reviewers)) ui_stop(paste(
        "Reviewer {ui_value(full_name)} already present.",
        "Please, provide a different one."
    ))

    attr(x, "reviewer_names") <- old_reviewers %>%
        c(purrr::set_names(full_name, rev_code))

    x[["reviewers"]] <- x[["reviewers"]] %>%
        dplyr::bind_rows(reviewer)

    set.seed(seed)
    if (!length(x[["used_data"]])) {
        x[["used_data"]] <- tibble(
            sampled_obs = nrow(x[["original_data"]]) %>%
                sample.int()
        )
    }

    x[["used_data"]][[rev_code]] <- factor(NA, levels = get_gold_classes(x))
    ui_done("Reviewer {ui_value(full_name)} added.")
    invisible(x)
}
