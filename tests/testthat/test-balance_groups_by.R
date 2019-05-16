context("test-balance_groups_by")


capture.output(
    balanced <- goldrake(mtcars) %>%
        balance_groups_by(vs)
)

test_that("balancing works", {

    expect_is(balanced, "goldrake")

    expect_equal(
      get_balanced_variables(balanced),
        "vs"
    )
})


test_that("error on wrong matching", {
    expect_error(
        goldrake(mtcars) %>%
            balance_groups_by(vs, foo)
    )
    expect_error(
        goldrake(mtcars) %>%
            balance_groups_by(foo)
    )
})
