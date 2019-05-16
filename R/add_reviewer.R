#' Add reviewer
#'
#' @param gold a \code{\link{goldrake}} object
#' @param name (chr) Reviewer's name
#' @param surname  (chr, default = NULL, optional) Reviewer's surname
#' @param inspector (lgl, default = FALSE) Is a reviewer a reviewer's
#'        reviewers?
#' @param seed (num, default = 1) seed for the case sampling
#'
#' @return invisibly, the updated version of gold
#' @export
#'
#' @examples
#' with_reviewer <- goldrake(mtcars) %>%
#'     set_gold_classes() %>%
#'     balance_groups_by(cyl, vs) %>%
#'     add_reviewer("Reviewer 1")
#'
#' with_reviewer
add_reviewer <- function(gold,
    name, surname = NULL, inspector = FALSE, seed = 1) {
    UseMethod("add_reviewer")
}

#' @rdname add_reviewer
#' @export
add_reviewer.default <- function(
    gold, name, surname = NULL, inspector = FALSE, seed = 1
) {
    ui_stop(paste(
        "The {ui_field('gold')} input in {ui_code('add_reviewer')}",
        "must be inherited from the {ui_value('goldrake')} class.\n",
        "It is of class {ui_value(class(gold))}.\n",
        "Please, provide an object of class {ui_value('goldrake')}."
    ))
}

#' @rdname add_reviewer
#' @export
add_reviewer.goldrake <- function(
    gold, name, surname = NULL, inspector = FALSE, seed = 1
) {
    if (length(name) != 1) ui_stop(paste(
        "You must {ui_field('add_reviewer')} for a single reviewer at time.\n",
        "Reviewer's name must be a single string."
    ))

    if (!length(get_gold_classes(gold))) ui_stop(paste(
        "{ui_code('gold')} has no gold classes set up.\n",
        "Please, set them by {ui_code('set_gold_classes()')} before to {ui_code('add_reviewer()')}"
    ))

    has_balance <- length(get_balanced_variables(gold))
    if (!has_balance) {
        if (ui_nope(paste0(
            "This goldrake has not been balanced by any variable.\n",
            "You cannot balance the selection after adding reviewers.\n",
            "\n",
            "Are you sure to continue without any balancing variables?"
        ))) {
            ui_fail("Process interrupted. Reviewer was not added.")
            return(invisible(gold))
        }
    }

    rev_id <- length(get_reviewers(gold)) + 1L
    rev_code <- paste0("rev_", rev_id)
    reviewer <- tibble::tibble(
        id   = rev_id,
        code = rev_code,
        name = name,
        surname = surname %||% NA_character_,
        inspector      = inspector %||% NA_character_
    )

    full_name <- stringr::str_c(name, surname, sep = ", ")

    old_reviewers <- get_reviewers(gold)
    if (any(full_name == old_reviewers)) ui_stop(paste(
        "Reviewer {ui_value(full_name)} already present.",
        "Please, provide a different one."
    ))

    attr(gold, "reviewer_names") <- old_reviewers %>%
        c(purrr::set_names(full_name, rev_code))

    gold[["reviewers"]] <- dplyr::bind_rows(gold[["reviewers"]], reviewer)

    set.seed(seed)
    if (!length(gold[["used_data"]])) {

        base_data <- gold[["original_data"]] %>%
            dplyr::mutate(.goldid = dplyr::row_number())

        if (has_balance) {
            base_data <- base_data %>%
                dplyr::group_by_at(get_balanced_variables(gold))
        }

        balanced <- base_data %>%
                dplyr::sample_frac() %>%
                dplyr::mutate(n = dplyr::row_number()) %>%
                dplyr::ungroup()

        gold[["used_data"]] <- tibble(
            sampled_obs = balanced[[".goldid"]][order(balanced[["n"]])]
        )
    }

    gold[["used_data"]][[rev_code]] <- factor(NA, levels = get_gold_classes(gold))
    ui_done("Reviewer {ui_value(full_name)} added.")

    invisible(gold)
}
