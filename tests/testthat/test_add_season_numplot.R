
library(agdesign)
context("test add season and plot number per trait")

dt<- data.frame(TraitName = paste0("TraitName",1:4),
                CropMeasurementPerSeason=c(1,5,1,3), 
                CropMeasurementPerPlot= c(1,1,4,6), stringsAsFactors = FALSE )

test_that("Test season =1 and plot =1", {
  
  lbl<- add_season_numplot_prefix(dt[c(1),])
  testthat::expect_equal(length(lbl) , 1)

})

test_that("Test season =2 and plot =1", {
  lbl<- add_season_numplot_prefix(dt[c(1,2),])
  testthat::expect_equal(length(lbl) , 6)

})
# 
# 
test_that("Test season =1 and plot =4", {
  lbl <- add_season_numplot_prefix(dt[c(1,2,3),])
  testthat::expect_equal(length(lbl) , 10)
})
# 
# 
test_that("Test season =3 and plot =6", {
  lbl <- add_season_numplot_prefix(dt[c(1,2,3,4),])
  testthat::expect_equal(length(lbl) , 28)

})