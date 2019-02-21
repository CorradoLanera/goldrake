gold_classes <- function(x) {
    UseMethod("gold_classes")
}

gold_classes.goldrake <- function(x) {
    attr(x, "gold_classes")
}







reviewer_names <- function(x) {
    UseMethod("reviewer_names")
}

reviewer_names.goldrake <- function(x) {
    attr(x, "reviewer_names")
}








balanced_variables <- function(x) {
    UseMethod("balanced_variables")
}

balanced_variables.goldrake <- function(x) {
    attr(x, "balanced_variables")
}








max_sampled <- function(x) {
    UseMethod("max_sampled")
}

max_sampled.goldrake <- function(x) {
    attr(x, "max_sampled")
}



