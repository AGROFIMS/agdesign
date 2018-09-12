

context("Checking addition levels as column in field trials")

test_that("add level as a column in CRD field trials", {

  library(agricolae)
  fb <- design.crd(trt=letters[1:10],r = 3)
  fb <- fb$book
  names(fb) <-  c("plots", "r", "trt")
  lvl <- "irrigado"
  fb <- add_cl(fb = fb, design_abr= "CRD", factor_lvl = lvl)
  expect_equal(names(fb), c("plots","r", "FACTOR", "trt"))

})

test_that("add level as a column in RCBD field trials", {

  library(agricolae)
  fb <- design.rcbd(trt=letters[1:10],r = 3)
  fb <- fb$book
  names(fb) <-  c("plot", "r", "trt")
  lvl <- "irrigado"
  fb <- add_cl(fb = fb, design_abr= "RCBD" ,factor_lvl = lvl)
  expect_equal(names(fb), c("plot", "r", "FACTOR", "trt"))

})


test_that("add level as a column in Augmented Block design (ABD) in field trials", {

  T1<-c("A","B","C","D")
  T2<-letters[20:26]
  outdesign <-design.dau(T1,T2, r=5,serie=2)
  fb <-outdesign$book
  names(fb) <-  c("plot", "block", "trt")
  lvl <- "irrigado"
  fb <- add_cl(fb = fb, design_abr= "ABD", factor_lvl = lvl)
  expect_equal(names(fb), c("plot","block", "FACTOR", "trt"))

})

test_that("add level as a column in Latin square design (LSD) in field trials", {

  varieties<-c("perricholi","yungay","maria bonita","tomasa")
  outdesign <-design.lsd(varieties,serie=2,seed=23)
  fb <- outdesign$book
  lvl <- "irrigado"
  fb <- add_cl(fb = fb, design_abr= "LSD", factor_lvl = lvl)
  expect_equal(names(fb), c("plots","row", "col", "FACTOR", "varieties"))

})


test_that("add level as a column in Wescott design (WD) in field trials", {

  out <- st4gi::cr.w(1:100, "A", "B", 100)
  fb <- out$book
  names(fb) <-  c("plot", "row", "col" , "trt")
  lvl <- "irrigado"
  fb <- add_cl(fb = fb, design_abr= "WD", factor_lvl = lvl)
  expect_equal(names(fb), c("plot", "row", "col", "FACTOR", "trt"))

})

test_that("add level as a column in Alpha Design (AD) field trials", {

    trt<-1:30
    t <- length(trt)
    # size block k
    k<-3
    # Blocks s
    s<-t/k
    # replications r
    r <- 2
    outdesign<- design.alpha(trt,k,r,serie=2)
    fb <-outdesign$book
    lvl <- "irrigado"
    fb <- add_cl(fb = fb, design_abr = "AD", factor_lvl = lvl)
    expect_equal(names(fb), c("plots","cols", "block", "FACTOR", "trt", "replication"))
})


















