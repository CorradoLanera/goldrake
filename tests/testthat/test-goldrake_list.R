context("test-goldrake_list")

test_that("goldrake_list return the correct object", {
    expect_equal(
        goldrake_list(tibble()),
        list(
            used_data     = tibble(),
            reviewers     = list(NULL),
            original_data = tibble()
        )
    )

    expect_equal(
    goldrake_list(mtcars),
    list(
        used_data     = tibble(),
        reviewers     = list(NULL),
        original_data = mtcars
    )
  )

})


test_that("goldrake_list return an error on wrong inputs", {
    expect_error(goldrake_list(1), "numeric")
})
