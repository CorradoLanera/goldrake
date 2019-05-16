gold <- goldrake(mtcars) %>%
    set_gold_classes() %>%
    balance_groups_by(cyl) %>%
    add_reviewer("Corrado", "L.")

test_that("return correct class", {
    skip_if(!interactive())
    expect_error(confirm_setup(1))
    expect_is(confirm_setup(gold), "goldrake")
})
