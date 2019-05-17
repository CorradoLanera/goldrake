#' Start to classify
#'
#' start to classify a \code{\link{goldrake}}.
#'
#' @param x a \code{\link{goldrake}}
#' @param gold_dir (chr, default = NULL) path to the project directory.
#'        If not provided the function automatically use (or create, if
#'        it not exists) the folder \code{.goldrake} in the current
#'        working directory.
#' @param gold_name (chr, default = NULL) the name of the project.
#'
#' @return the update version of \code{\link{goldrake}} x
#' @export
#'
#' @examples
#' \dontrun{
#'     gold <- goldrake(mtcars) %>%
#'         set_gold_classes() %>%
#'         add_reviewer("Rev 1") %>%
#'         confirm_setup()
#'
#'     start_classify(gold)
#' }
start_classify <- function(x = NULL, gold_dir = NULL, gold_name = NULL) {
    UseMethod("start_classify")
}


#' @rdname start_classify
#' @export
start_classify.NULL <- function(x = NULL, gold_dir = NULL, gold_name = NULL) {
    default_path <- gold_dir %||% here::here(".goldrake")

    if (!dir.exists(default_path)) {
        ui_fail("{ui_code(default_path)} does not exists.")
        ui_todo("Please, provide the path to an existing directory containing the stored goldrake")
        ui_fail("Nothing is changed.")
        return(invisible(x))
    }

    possible_goldrake <- list.files(default_path, "\\.goldrake$") %>%
        sort()

    if (!length(possible_goldrake)) {
        ui_fail("{ui_code(default_path)} hasn't any goldrake file.")
        ui_fail("Please, provide a path to a directory containing at least one goldrake file.")
        ui_fail("Nothing is changed.")
        return(invisible(x))
    }

    if (!is.null(gold_name) && (gold_name %in% possible_goldrake)) {
        to_load <- gold_name
    } else {
        option_to_load <- c(possible_goldrake, "exit")
        to_load <- option_to_load[ui_select(
            "Which file do you want to load?", option_to_load
        )]

        if (to_load == "exit") {
            ui_fail("Good bye.")
            return(invisible(x))
        }
    }

    loaded_goldrake <- readRDS(file.path(default_path, to_load))
    ui_done("{ui_code(to_load)} loaded.")

    start_classify(loaded_goldrake, default_path, to_load)
}


#' @rdname start_classify
#' @export
start_classify.goldrake <- function(
    x, gold_dir = NULL, gold_name = NULL
) {
    possible_review <- c(get_reviewers(x), "exit") %>%
        purrr::set_names()
    reviewer <- possible_review[ui_select("Who are you?", possible_review)]

    if (reviewer == "exit") {
        ui_fail("Nothing is changed.")
        ui_fail("Good bye.")
        return(invisible(x))
    }

    ui_done("{reviewer} set as the actual reviewer.")
    rev_code <- names(reviewer)

    to_review <- get_to_review(x, rev_code)
    skip <- 0L
    while (nrow(to_review) != sum(stats::complete.cases(to_review))) {

        to_review    <- get_to_review(x, rev_code)
        are_missings <- is.na(to_review[[rev_code]])

        if (skip >= sum(are_missings)) {
            ui_fail("You reach the end of the database with {ui_value(skip)} records skipped.")
            if (skip && ui_yeah("Do you want to continue with the classificaton of the skipped records?")) {
                skip     <- 0L
                next
            } else {
                break
            }
        }

        first_missing <- which(are_missings)[[1L + skip]]

        var_to_show <- names(get_original_data(x)) %>%
            setdiff(get_balanced_variables(x))

        data_selected <- get_original_data(x)[to_review[[first_missing, "sampled_obs"]], , drop = FALSE]
        data_selected[var_to_show] %>% purrr::iwalk(~ui_todo("{.y}: {.x}."))

        ui_todo('Seleziona la classe:')
        class_options <- c(get_gold_classes(x), "save & skip", "save & exit", "exit w/o save")
        selected_class <- class_options[[ui_select(" ", class_options)]]

        if (selected_class == "exit w/o save") {
            if (ui_yeah("Are you sure do you want to exit WITHOUT saving? If so, you will lose all your unsaved work.")) {
                ui_fail("Nothing is changed on disk.")
                break
            }
            next
        }

        if (selected_class == "save & exit") {
            if (is.null(gold_dir)) {
                ui_todo("A default directory is not setup, please select one")
                gold_dir <- choose.dir()
            }
            if (is.null(gold_name)) {
                ui_todo("A default file name is not setup, please select one")
                gold_name <- readline("Type the name of your file.")
            }

            if (ui_nope("Your data will be written in {path(gold_dir, gold_name)}. Do you agree")) {
                ui_fail("Nothing is changed on disk.")
                next
            }

            ui_todo("Saving data in {path(gold_dir, gold_name)}...")
            saveRDS(x, file = path(gold_dir, gold_name))
            ui_done("A local copy of {ui_value('gold_name')} has been saved on disk.")
            break
        }


        if (selected_class == "save & skip") {
            if (is.null(gold_dir)) {
                ui_todo("A default directory is not setup, please select one")
                gold_dir <- choose.dir()
            }
            if (is.null(gold_name)) {
                ui_todo("A default file name is not setup, please select one")
                gold_name <- readline("Type the name of your file.")
            }

            if (ui_nope("Your data will be written in {path(gold_dir, gold_name)}. Do you agree")) {
                ui_fail("Nothing is changed on disk.")
                next
            }

            ui_todo("Saving data in {path(gold_dir, gold_name)}...")
            saveRDS(x, file = path(gold_dir, gold_name))
            ui_done("A local copy of {ui_value('gold_name')} has been saved on disk.")

            skip <- skip + 1L
            next
        }


        x[["used_data"]][[first_missing, rev_code]] <- get_gold_classes(x)[[match(selected_class, get_gold_classes(x))]]
        ui_done("Class set: {ui_value(selected_class)}.")
    }


    if (skip) {
        ui_fail("Exiting with {ui_value(skip)} record skipped by {ui_value(reviewer)}")
        ui_todo("Don't worry, you can complete your work with a future call to {ui_code('start_classify()')}!")
    }


    left_missing <- length(are_missings)
    if (left_missing) {
        ui_fail("There are {left_missing} records left to classify.")
        ui_todo("Don't worry, you can complete your work with a future call to {ui_code('start_classify()')}!")
    } else {
        ui_done("Classification completed for {ui_value(reviewer)}")
        ui_done("Great Job!")
    }


    ui_todo("Saving data...")
    path_is_to_set <- is.null(gold_dir) || is.null(gold_name)
    go <- FALSE
    while (!go) {
        go <- TRUE

        while (path_is_to_set) {
            if (is.null(gold_dir)) {
                ui_todo("Please, select a directory")
                gold_dir <- choose.dir()
            }
            if (is.null(gold_name)) {
                ui_todo("Please, select a file name")
                gold_name <- readline("Type the name of your goldrake file.")
            }

            if (ui_nope("Your data will be written in {path(gold_dir, gold_name)}. Do you agree")) {
                gold_dir  <- NULL
                gold_name <- NULL
            }

            go <- FALSE
        }

        if (go && ui_nope("Your data will be written in {path(gold_dir, gold_name)}. Do you agree")) {

            gold_dir  <- NULL
            gold_name <- NULL

            go <- FALSE
        }
    }

    ui_todo("Saving data in {path(gold_dir, gold_name)}...")
    saveRDS(x, file = path(gold_dir, gold_name))
    ui_done("A local copy of {ui_value('gold_name')} has been saved on disk.")

    ui_done("Thank you! Bye.")
    invisible(x)
}
