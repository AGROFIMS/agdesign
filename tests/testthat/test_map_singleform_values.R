library(agdesign)

context("test map_singleform_values")

#----------- type = select  --------------------------------------------------------------

test_that("To test with input equal to NULL and type = select", {
  out<- map_singleform_values(input= NULL, type="select", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test with input equal to double quotes '' and type = select", {
  out<- map_singleform_values(input= "", type="select", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test with input equal to vector and type = select, but, not without collapsing", {
  v <- letters[1:10]
  out<- map_singleform_values(input= v, type="select", multiple=FALSE, collapsed= FALSE)
  expect_equal(out, v)
})

test_that("To test with input equal to NA", {
  out<- map_singleform_values(input= NA, type="select", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})


test_that("To test with vector input equal to NAs", {
  out<- map_singleform_values(input= c(NA,2,NA), type="select", multiple=FALSE, collapsed= FALSE)
  expect_equal(out, "")
})

#-----------type = date   --------------------------------------------------------------

test_that("To test with input equal to NULL and type = date", {
  out<- map_singleform_values(input= NULL, type="date", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("input equal to double quotes '' and type = date", {
  out<- map_singleform_values(input= "", type="date", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test with one date equal to character(0)", {
  v <- character(0)
  out<- map_singleform_values(input= v, type="select", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test with many dates equal to character(0)", {
  v <- c(character(0),character(0))
  out<- map_singleform_values(input= v, type="select", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test with input equal to NA", {
  out<- map_singleform_values(input= NA, type="select", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})


#----------- type = text --------------------------------------------------------------

test_that("To test with input equal to NULL and type = text", {
  out<- map_singleform_values(input= NULL, type="text", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test with input equal to double quotes '' and type = text", {
  out<- map_singleform_values(input= "", type="text", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "")
})

test_that("To test trimming whitespaces from strings", {
  out<- map_singleform_values(input= " bla blab bla bla ", type="text", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "bla blab bla bla")
})

test_that("test collapse by commas", {
  out<- map_singleform_values(input= letters[1:10], type="text", multiple=FALSE, collapsed= TRUE)
  expect_equal(out, "a, b, c, d, e, f, g, h, i, j")
})

