#' Confirm goldrake setup
#'
#' @param x a goldrake object
#' @param prj_name (chr, default = "goldrake") name for the project
#'
#' @return invisibly(x), moreover it save a local copy of goldrake
#' @export
#'
#' @examples
#' \dontrun{
#'     library(goldrake)
#'     gold <- goldrake(mtcars) %>%
#'         set_gold_classes() %>%
#'         add_reviewer("Rev 1")
#'
#'     confirm_setup(gold)
#' }
confirm_setup <- function(x, prj_name = "goldrake") {
    UseMethod("confirm_setup")
}


#' @rdname confirm_setup
#' @export
confirm_setup.goldrake <- function(x, prj_name = "goldrake") {

    print(x)

    if (ui_nope("Are all the information correct?")) {
        ui_fail("Please, revise and correct your goldrake object.")
        ui_fail("Nothing has been written on your disk.")
        return(invisible(x))
    }


    if (ui_nope(
        "Do you agree to write a local copy of your current goldrake?")
    ) {
        ui_fail("Nothing has been written on your disk.")
        ui_done("Check completed: now you can {ui_code('start_classify()')}")
        return(invisible(x))
    }

    g_path <- here::here(".goldrake")
    create_directory(g_path)

    saveRDS(x,
        file = path(g_path,
            paste0(format(Sys.time(), "%Y%m%d%H%M"), "-", prj_name),
            ext = "goldrake"
        )
    )

    ui_done("Setup completed: now you can {ui_code('start_classify()')}")
    invisible(x)
}
