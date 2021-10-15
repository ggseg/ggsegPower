library(ggseg)
library(ggseg3d)
library(ggplot2)

# ggseg ----
context("test-palettes")
test_that("check new palettes work", {
  expect_equal(length(brain_pal("power", package = "ggsegPower")), 130)

  expect_error(brain_pal("power"), "not a valid")

  expect_true(all(brain_regions(power) %in% names(brain_pal("power", package = "ggsegPower"))))
})

context("test-ggseg-atlas")
test_that("atlases are true ggseg atlases", {

  expect_true(is_brain_atlas(power))

})

context("test-ggseg")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = power),c("gg","ggplot"))

  expect_is(ggseg(atlas = power, mapping = aes(fill = region)),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = power, mapping = aes(fill = region)) +
              scale_fill_brain("power", package = "ggsegPower"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = power, mapping = aes(fill = region)) +
              scale_fill_brain("power", package = "ggsegPower"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = power, mapping=aes(fill=region), adapt_scales = FALSE ),c("gg","ggplot"))

})


# ggseg3d ----
context("test-ggseg3d")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas=power_3d),
    c("plotly", "htmlwidget")
  )
})



context("test-ggseg3d-atlas")
test_that("atlases are true ggseg3d atlases", {

  expect_true(is_ggseg3d_atlas(power_3d))

})
