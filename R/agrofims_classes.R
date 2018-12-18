# 
# cropMeasurements <- data.frame(
#   name = c('yield', 'number of tubers', 'biomass'),
#   scale = c('kg/ha', '-', 'kg/ha')
# )
# 
# setClass('Measurements',
#   representation (
#     d = 'data.frame'        
#   ), 
#   prototype (
#     d = cropMeasurements
#   )
# )
# 
# setClass('Person',
#          representation (
#            name = 'character',
#            affiliation = 'character',
#            position = 'character',
#            ORCID = 'character'
#          )
# )
# 
# setClass('Person2',
#    representation (
#       d = 'data.frame'
#    ), 
#    prototype (
#       d = data.frame(name=NA, affiliation=NA)[-1,]
#    ),
#    validity = function(object)	{
#      all(colnames(object) == c("name", "affiliation"))
#    }
# )
# 
# setClass('ExperimentDetails',
#     representation (
#            id = 'character',
#            name = 'character',
#            project = 'character',
#            startDate = 'Date',
#            endDate = 'Date',
#            type = 'character',
#            objective = 'character'
#     )
# )
# 
# 
# setClass('Experiment',
#     representation (
#           person = 'Person2',
#            details = 'ExperimentDetails',
#           measurements = 'list'
#     )
# )
# 
# 
# setMethod ('show' , 'Experiment', 
#            function(object) {
#               str(object)           
#           }
# )	
# 
# 
# 
# if (!isGeneric("check")) {setGeneric("check", function(x, ...) standardGeneric("check")) }
# 
# setMethod ("check" , "ExperimentDetails", 
#    function(x, ...) {
#       test <- ""
#       test <- (x@startDate < x@endDate)
#       if (!test) return("start date >= end date")
#       test <- test & (x@startDate > as.Date("1900-01-01"))
#       if (!test) return("start date before 20th century")
#       test
#    }
# )	
# 
# experiment <- function(person, details) {
#   exp <- new("Experiment")
#   exp@person <- person
#   exp@details <- details
#   exp
# }
# 
# exp_dt <- function(input) {
#   x <- new("ExperimentDetails")
# #  nms <- names(input)
# #  nms <- slotNames(x)
#   txtnms <- c('id', 'name', 'project', 'type', 'objective')
#   for (n in txtnms) {
#     slot(x, n) <- input[[n]]
#   }
#   datenms <- c('startDate', 'endDate') 
#   for (n in datenms) {
#     slot(x, n) <- as.Date(input[[n]], "%Y%m%d")
#   }
#   x
# }
# 
# exp <- list(id="F2009_afg", name = "hola", project="LB breeding",
#           startDate="10180408", endDate="20180918", 
#           type="Contr treat", objective="Get better stuff")
# 
# 
# 
# r <- exp_dt(exp)
# x <- check(r)
# if (!isTRUE(x)) warning(x)
# 
# person2 <- function(p, ...) {
#   x <- new("Person2")
#   nms <- names(p)
#   pp <- c(list(p), list(...))
#    for (row in 1:length(pp)) {
#     for (n in nms) {
#       x@d[row, n] <- pp[[row]][[n]]
#     }
#   }
#   x
# }
# 
# 
# fx <- function(p, ..., classname) {
#   x <- new(classname)
#   nms <- names(p)
#   pp <- c(list(p), list(...))
#   for (row in 1:length(pp)) {
#     for (n in nms) {
#       x@d[row, n] <- pp[[row]][[n]]
#     }
#   }
#   x
# }
# 
# 
# p <- fx(list(name=c("Ivan"), affiliation=c("CIP")),
#              list(name=c("Omar"), affiliation=c("CIAT")),
#              list(name=c("Susan"), affiliation=c("IRRI")),
#         classname = "Person2"
# )
# 
# e <- experiment(p, r)
# e
