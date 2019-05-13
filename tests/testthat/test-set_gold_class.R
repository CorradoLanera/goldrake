context("test-set_gold_classes")

capture.output(
    default_classes <- goldrake(mtcars) %>%
        set_gold_classes(),

    user_classes <- goldrake(mtcars) %>%
        set_gold_classes(c("good", "bad", "so and so"))
)




test_that("assginment works", {
    expect_is(default_classes, "goldrake")
    expect_equal(get_gold_classes(default_classes), c("0", "1"))

    expect_is(user_classes, "goldrake")
    expect_equal(
        get_gold_classes(user_classes),
        c("good", "bad", "so and so")
    )
})


test_that("reassignment work", {
    expect_warning(
        default_classes %>% set_gold_classes()
    )

    expect_is(
        suppressWarnings(
            default_classes %>% set_gold_classes()
        ),
        "goldrake"
    )

    expect_equal(
        suppressWarnings(
            user_classes %>%
                set_gold_classes() %>%
                get_gold_classes()
        ),
        c("good", "bad", "so and so")
    )

    expect_equal(
        suppressWarnings(
            user_classes %>%
                set_gold_classes(force = TRUE) %>%
                get_gold_classes()
        ),
        c("0", "1")
    )
})
