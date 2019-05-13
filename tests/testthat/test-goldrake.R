context("test-goldrake")

empty_goldrake   <- goldrake(tibble())






# class -----------------------------------------------------------


test_that("is a goldrake object", {
  expect_s3_class(empty_goldrake, "goldrake")
})




# input -----------------------------------------------------------


test_that("goldrake require the correct inputs", {
    expect_equal(formals(goldrake) %>% names(), "original_data")
})


test_that("goldrake throw an error on wrong input", {
    expect_condition(goldrake(1), "numeric", class = "goldrake_error")
})








# components ------------------------------------------------------


test_that("empty goldrake has the expected components", {
    component_names <- c("used_data", "reviewers", "original_data")

    expect_equal(names(empty_goldrake), component_names)

    expect_is(empty_goldrake[["used_data"]], "tbl_df")
    expect_length(empty_goldrake[["reviewers"]], 0L)
    expect_s3_class(
        empty_goldrake[["original_data"]],
        c("tbl_df", "data.frame")
    )
})








# attributes ------------------------------------------------------


test_that("empty goldrake has expected attributes", {

    attr_names <- c(
        "names",
        "reviewer_names", "gold_classes", "balanced_variables",
        "max_sampled",
        "class"
    )

    expect_equal(attributes(empty_goldrake) %>% names(), attr_names)

    expect_is(attr(empty_goldrake, "reviewer_names"), "character")
    expect_length(attr(empty_goldrake, "reviewer_names"), 0)

    expect_is(attr(empty_goldrake, "gold_classes"), "character")
    expect_length(attr(empty_goldrake, "gold_classes"), 0)

    expect_is(attr(empty_goldrake, "balanced_variables"), "character")
    expect_length(attr(empty_goldrake, "balanced_variables"), 0)
    expect_true(all(
        attr(empty_goldrake, "balanced_variables") %in%
        names(empty_goldrake[["used_data"]])
    ))

})






