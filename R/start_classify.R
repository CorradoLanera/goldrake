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
        to_load <- ui_select("Which file do you want to load?",
            possible_goldrake
        )

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
    reviewer <- ui_select("Who are you?", get_reviewers(x))
    if (reviewer == "exit") {
        ui_fail("Nothing is changed.")
        ui_fail("Good bye.")
        return(invisible(x))
    }

    reviewer <- get_reviewers(x)[get_reviewers(x) == reviewer]

    ui_done("{reviewer} set as the actual reviewer.")

    rev_code <- names(reviewer)

    to_review <- get_to_review(x, rev_code)



    again <- TRUE
    progress <- 1L
    while (
        (nrow(to_review) != sum(stats::complete.cases(to_review))) &&
        again
    ) {
        are_missings <- is.na(to_review[[rev_code]])
        if (progress > sum(are_missings)) {
            ui_fail("You reach the end of the database with {ui_value(progress - 1)} record skipped.")
            if (ui_yeah("Do you want to continue with the classificaton of the skipped records?")) {
                progress <- 1
            } else {
                ui_todo("Stop classification with {ui_value(progress - 1)} records left to be classified.")
                break
            }
        }

        first_missing <- which(are_missings)[[progress]]

        var_to_show <- names(get_original_data(x)) %>%
            setdiff(get_balanced_variables(x))

        print(unclass(
            get_original_data(x)[first_missing, var_to_show, drop = FALSE]
        ))

        ui_todo('Seleziona la classe.')
        selected_class <- ui_select(" ", get_gold_classes(x))

        if (selected_class == "exit") {
            if (ui_yeah("Do you want to skip the next record?")) {
                progress <- progress + 1L
                next
            } else {
                if (ui_yeah("Do you want to update the local copy of your goldrake with the work progresses already done?")) {
                    saveRDS(x, file = path(gold_dir, gold_name))
                    ui_done("The local copy of {ui_value('gold_name')} has been updated on disk.")
                } else {
                    ui_fail("Nothing is changed on disk.")
                }
                ui_fail("Good bye.")
                return(invisible(x))
            }
        }

        x[["used_data"]][[first_missing, rev_code]] <- get_gold_classes(x)[[match(selected_class, get_gold_classes(x))]]
        ui_done("Class set: {ui_value(selected_class)}.")
        progress <- progress + 1L

        again <- ui_yeah("Do you want to procede with the next one?")
        if (again) {
            progress <- progress + 1L
        }
    }

    if (progress != 1) {
        ui_fail("Exiting with {ui_value(progress - 1L)} record skipped by {ui_value(reviewer)}")
        ui_todo("Don't worry, you can complete your work with a future call to {ui_code('start_classify()')}!")
    }

    if (again) {
        ui_done("Classification completed for {ui_value(reviewer)}")
        ui_done("Great Job!")
    } else {
        ui_fail("Don't worry, you can complete your work with a future call to {ui_code('start_classify()')}!")
    }

    if (ui_yeah("Do you want to update the local copy of your goldrake with the work progresses already done?")) {
        saveRDS(x, file = path(gold_dir, gold_name))
        ui_done("The local copy of {ui_value('gold_name')} has been updated on disk.")
    } else {
        if (ui_nope("Are you sure you do NOT want to update the local copy of your goldrake with the work progresses already done? (if YES, all the work of the current session will be definitely lost!)")) {
            saveRDS(x, file = path(gold_dir, gold_name))
            ui_done("The local copy of {ui_value('gold_name')} has been updated on disk.")
        }
        ui_fail("Nothing is changed on disk.")
    }

    ui_done("Thank you.")
    invisible(x)
}
