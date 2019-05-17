#' User interface
#'
#' The `ui_` functions can be broken down into four main categories:
#'
#' * block styles : `ui_line()`, `ui_done()`, `ui_todo()`.
#' * conditions   : `ui_stop()`, `ui_warn()`.
#' * questions    : `ui_yeah()`, `ui_nope()`.
#' * inline styles: `ui_path()`, `ui_code()`, `ui_field()`,
#'                  `ui_value()`.
#'
#' @param x A character vector.
#'
#'   For block styles, conditions, and questions, each element of the
#'   vector becomes a line, and the result is processed by
#'   [glue::glue()]. For inline styles, each element of the vector
#'   becomes an entry in a comma separated list.
#' @param .envir Used to ensure that [glue::glue()] gets the correct
#'   environment. For expert use only.
#' @return The block styles, conditions, and questions are called for
#'   their side-effect. The inline styles return a string.
#' @keywords internal
#' @name ui
NULL

# Block styles ------------------------------------------------------------

#' @rdname ui
ui_line <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_line(x)
}

#' @rdname ui
ui_todo <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_bullet(x, crayon::red(clisymbols::symbol$bullet))
}

#' @rdname ui
ui_done <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_bullet(x, crayon::red(crayon::green(clisymbols::symbol$tick)))
}


#' @rdname ui
ui_fail <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_bullet(x, crayon::red(clisymbols::symbol$cross))
}


#' @param copy If `TRUE`, the session is interactive, and the clipr package
#'   is installed, will copy the code block to the clipboard.
#' @rdname ui
ui_code_block <- function(x, copy = interactive(), .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  block <- indent(x, "  ")
  block <- crayon::make_style("darkgrey")(block)
  cat_line(block)

  if (copy && clipr::clipr_available()) {
    x <- crayon::strip_style(x)
    clipr::write_clip(x)
    cat_line("  [Copied to clipboard]")
  }
}

# Conditions --------------------------------------------------------------

#' @rdname ui
ui_stop <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  cnd <- structure(
    class = c("goldrake_error", "error", "condition"),
    list(message = x)
  )

  stop(cnd)
}

#' @rdname ui
ui_warn <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  warning(x, call. = FALSE, immediate. = TRUE)
}

# Questions ---------------------------------------------------------------

#' @rdname ui
ui_yeah <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  if (!interactive()) {
    ui_stop(c(
      "User input required, but session is not interactive.",
      "Query: {x}"
    ))
  }

  ayes <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely")
  nays <- c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not")

  qs <- c(sample(ayes, 1), sample(nays, 2))
  ord <- sample(length(qs))

  cat_line(x)
  out <- utils::menu(qs[ord])
  out != 0L && (ord == 1)[[out]]
}

#' @rdname ui
ui_nope <- function(x, .envir = parent.frame()) {
  !ui_yeah(x, .envir = .envir)
}


#' @rdname ui
ui_select <- function(x, options, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  if (!interactive()) {
    ui_stop(c(
      "User input required, but session is not interactive.",
      "Query: {x}"
    ))
  }

  cat_line(x)
  utils::menu(options)
}


# Inline styles -----------------------------------------------------------

#' @rdname ui
ui_field <- function(x) {
  x <- crayon::green(x)
  x <- glue::glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui
ui_value <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }
  x <- crayon::blue(x)
  x <- glue::glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui
#' @param base If specified, paths will be displayed relative to this path.
ui_path <- function(x, base = NULL) {
  is_directory <- is_dir(x)
  if (is.null(base)) {
    x <- here(x)
  } else if (!identical(base, NA)) {
    x <- path_rel(x, base)
  }

  x <- paste0(x, ifelse(is_directory, "/", ""))
  x <- ui_value(x)
  x
}

#' @rdname ui
ui_code <- function(x) {
  x <- encodeString(x, quote = "`")
  x <- crayon::make_style("darkgrey")(x)
  x <- glue::glue_collapse(x, sep = ", ")
  x
}

# Cat wrappers ---------------------------------------------------------------

cat_bullet <- function(x, bullet) {
  bullet <- paste0(bullet, " ")
  x <- indent(x, bullet, "  ")
  cat_line(x)
}

cat_line <- function(...) {
  lines <- paste0(..., "\n")
  cat(lines, sep = "")
}
