get_gold_classes <- function(x) {
    UseMethod("get_gold_classes")
}

get_gold_classes.goldrake <- function(x) {
    attr(x, "gold_classes")
}







get_reviewers <- function(x) {
    UseMethod("get_reviewers")
}

get_reviewers.goldrake <- function(x) {
    attr(x, "reviewer_names")
}





get_original_data <- function(x) {
    UseMethod("get_original_data")
}

get_original_data.goldrake <- function(x) {
    x[["original_data"]]
}





get_used_data <- function(x) {
    UseMethod("get_used_data")
}

get_used_data.goldrake <- function(x) {
    x[["used_data"]]
}








get_balanced_variables <- function(x) {
    UseMethod("get_balanced_variables")
}

get_balanced_variables.goldrake <- function(x) {
    attr(x, "balanced_variables")
}








get_max_sampled <- function(x) {
    UseMethod("get_max_sampled")
}

get_max_sampled.goldrake <- function(x) {
    attr(x, "max_sampled")
}





get_to_review <- function(x, rev_code) {
    UseMethod("get_to_review")
}

get_to_review.goldrake <- function(x, rev_code) {
    used_data <- get_used_data(x)

    used_data[setdiff(
        names(used_data),
        setdiff(names(get_reviewers(x)), rev_code)
    )]
}





get_done <- function(x) {
    UseMethod("get_done")
}

get_done.goldrake <- function(x) {
    get_used_data(x) %>%
        dplyr::select(x[["reviewers"]][["code"]]) %>%
        janitor::remove_empty("rows")
}


