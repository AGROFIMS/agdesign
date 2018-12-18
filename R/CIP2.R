# 
# setClass('Person',
# 	representation (
# 		name = 'character',
# 		affiliation = 'character',
# 		position = 'character',
# 		ORCID = 'character'
# 	)
# )
# 
# setClass('PersonVector',
# 	representation (
# 		v = 'list'
# 	)
# )
# 
# setMethod ('length' , 'PersonVector', 
# 	function(x) {
# 		length(x@v)
# 	}
# )	
# 
# setMethod ('names' , 'PersonVector', 
# 	function(x) {
# 		sapply(x@v, function(i) i@name)
# 	}
# )	
# 
# setClass('AgTrial',
# 	representation (
# 		principal_investigator = 'PersonVector',
# 		presupuesto = 'numeric'
# 	),
# 	prototype (	
# 		presupuesto = 10
# 	),
# 	validity = function(object)	{
# 		object@presupuesto > 0
# 	}
# )
# 
# #contains = c('BasicRaster', 'VIRTUAL')
# 
# setClass('BreedingTrial', contains = 'AgTrial',
# 	representation (
# 		ADN = 'data.frame'
# 	),
# )
# 
# 
# 
# setMethod ('show' , 'AgTrial', 
# 	function(object) {
# 		cat('class       :' , class(object), '\n')
# 		cat('PI          :' , object@principal_investigator@name, '\n')
# 		cat('presupuesto :' , object@presupuesto, '\n')
# 	}
# )	
# 
# if (!isGeneric("mult10")) {setGeneric("mult10", function(x, ...) standardGeneric("mult10")) }
# 
# setMethod ('mult10' , 'AgTrial', 
# 	function(x, ...) {
# 		x@presupuesto * 10
# 	}
# )	
# 
# 
# 
# createTrial <- function(name, presupuesto) {
# 	x <- new("AgTrial")
# 	x@principal_investigator@name = name
# 	x@presupuesto = presupuesto
# 	x
# }
# 
# setMethod ('show' , 'BreedingTrial', 
# 	function(object) {
# 		cat('class       :' , class(object), '\n')
# 		cat('director    :' , object@principal_investigator@name, '\n')
# 		cat('presupuesto :' , object@presupuesto, '\n')
# 		cat('ADN\n')
# 		print(head(object@ADN, 3))
# 	}
# )	
