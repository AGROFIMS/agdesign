library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test weather and soil tables")


test_that("One variable in the table for weather and soil variables", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "dt_soil_weather_inputs.rds")
  allinputs <- readRDS(fname)
 
  addId <- "1"
  dt_w <- get_weather_variables(allinputs,addId = addId)
  dt_soil<- get_soil_variables(allinputs,addId = addId)
   
  #row filtered
  testthat::expect_equal(nrow(dt_w), 1)
  testthat::expect_equal(nrow(dt_soil), 1)
  
})

test_that("One variable in the table for weather and soil variables", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "dt_soil_weather_inputs.rds")
  allinputs <- readRDS(fname)
  
  addId <- c("1")
  dt_w <- get_weather_variables(allinputs,addId = addId)
  dt_soil<- get_soil_variables(allinputs,addId = addId)
  
  #row filtered
  testthat::expect_equal(nrow(dt_w), 1)
  testthat::expect_equal(nrow(dt_soil), 1)
  
})

test_that("Two variables selected in the Weather Tab", {
  fname <- rprojroot::find_testthat_root_file("userInput", "weather_vars_case2.rds")
  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  allinputs <- readRDS(fname)
  
  addId <- c("1","2","3")
  dt1 <- get_weather_variables(allinputs,addId = addId)
  dt1_join<- get_dt_weather(dt1, dt_weather = dt_weather)

  testthat::expect_equal(nrow(dt1),3)
  testthat::expect_equal(nrow(dt1_join),3)
  
})


test_that("Two variables selected in the Soil Tab", {
  fname <- rprojroot::find_testthat_root_file("userInput", "soil_vars_case2.rds")
  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  allinputs <- readRDS(fname)
  
  addId <- c("1")
  dt1 <- get_soil_variables(allinputs,addId = addId)
  dt1_join<- get_dt_soil(dt1, dt_soil = dt_soil)
  
  cs<- add_season_numplot_prefix(dt=dt1_join)
  
  testthat::expect_equal(nrow(dt1),1)
  testthat::expect_equal(nrow(dt1_join),1)
  
})


test_that("Test add_season_numplot for weather variables", {
  fname <- rprojroot::find_testthat_root_file("userInput", "soil_vars_case2.rds")
  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  allinputs <- readRDS(fname)
  addId <- c("1")
  dt1 <- get_soil_variables(allinputs,addId = addId)
  dt1_join<- get_dt_soil(dt1, dt_soil = dt_soil)
  
  cs<- add_season_numplot_prefix(dt=dt1_join)
  
  testthat::expect_equal(length(cs),1)
  testthat::expect_false(anyNA(cs))
  
})

test_that("Remove variables from weather and soil", {
  fname <- rprojroot::find_testthat_root_file("userInput", "soil_vars_case3.rds")
  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  allinputs <- readRDS(fname)
  addId <- character(0)
  dt1 <- get_soil_variables(allinputs,addId = addId)
  dt1_join<- get_dt_soil(dt1, dt_soil = dt_soil)
  
  #cs<- add_season_numplot_prefix(dt=dt1_join)
  
  
  testthat::expect_equal(nrow(dt1),0)
  testthat::expect_equal(nrow(dt1_join),0)
  # testthat::expect_equal(length(cs),0)
  # testthat::expect_false(anyNA(cs))
  
})

