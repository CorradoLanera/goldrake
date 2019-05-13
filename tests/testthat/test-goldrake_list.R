context("test-goldrake_list")

empty_rev <- tibble(
  id = integer(),
  code = character(),
  name = character(),
  surname = character(),
  inspector = logical()
)

test_that("goldrake_list return the correct object", {
    expect_equal(
        goldrake_list(tibble()),
        list(
            used_data     = tibble(),
            reviewers     = empty_rev,
            original_data = tibble()
        )
    )

    expect_equal(
    goldrake_list(mtcars),
    list(
        used_data     = tibble(),
        reviewers     = empty_rev,
        original_data = mtcars
    )
  )

})


test_that("goldrake_list return an error on wrong inputs", {
    expect_condition(goldrake_list(1), "numeric", class = "simpleError")
})
