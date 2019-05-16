capture.output({
    gold <- goldrake(mtcars) %>%
        set_gold_classes() %>%
        balance_groups_by(cyl)
})

test_that("add reviewer class", {
    expect_is(add_reviewer(gold, "rev 1"), "goldrake")
})

test_that("get methods", {
    assigned <- add_reviewer(gold, "Reviewer 1")
    expect_equal(get_reviewers(assigned), c(rev_1 = "Reviewer 1"))
})

test_that("name and surname correctly setted", {
    expect_equal(
        add_reviewer(gold, "my name") %>% get_reviewers(),
        c(rev_1 = "my name")
    )
    expect_equal(
        add_reviewer(gold, "my name", "surname") %>% get_reviewers(),
        c(rev_1 = "my name, surname")
    )
    expect_equal(
        gold %>%
            add_reviewer("my name", "surname") %>%
            add_reviewer("your name", "surname") %>%
            get_reviewers(),
        c(rev_1 = "my name, surname", rev_2 = "your name, surname")
    )
})

test_that("possible errors", {
    expect_error(add_reviewer(1), "must be inherited from",
        class = "goldrake_error"
    )

    expect_error(
        goldrake(mtcars) %>% add_reviewer("rev1"),
        "has no gold classes set up",
        class = "goldrake_error"
    )

    expect_error(
        gold %>%
            add_reviewer(c("1", "2")),
        "single string",
        class = "goldrake_error"
    )

    expect_error(
        gold %>%
            add_reviewer("1") %>%
            add_reviewer("1"),
        "already present",
        class = "goldrake_error"
    )
})


test_that("Correct print", {
    with_reviewer <- add_reviewer(gold, "Reviewer 1", "Surname")

    expect_true(
        capture.output(with_reviewer) %>%
            stringr::str_detect("Reviewer 1, Surname") %>%
            any()
    )
})


test_that("Correctly add reviewer metadata", {
    one_rev <- gold %>%
        add_reviewer("rev 1")

    two_rev <- gold %>%
        add_reviewer("rev 1") %>%
        add_reviewer("rev 2")

    expect_is(one_rev[["reviewers"]], "tbl_df")
    expect_length(one_rev[["reviewers"]], 5L)
    expect_equal(nrow(one_rev[["reviewers"]]), 1L)
    expect_equal(nrow(two_rev[["reviewers"]]), 2L)
    expect_equal(two_rev[["reviewers"]][["id"]][[2]], 2L)

    expect_equal(
        levels(one_rev[["used_data"]][["rev_1"]]),
        c("0", "1")
    )

    expect_true(
        all(c("rev_1", "rev_2") %in% names(two_rev[["used_data"]]))
    )

})
