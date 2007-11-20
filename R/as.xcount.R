`as.xcount` <-
function (table, species.columns=TRUE, segment="unspecified"){
#if(!is.integer(as.integer(as.matrix(table)))) stop("Count must be integer!")
if(species.columns == FALSE) table <- t(table)
table <- table[order(rownames(table)), order(colnames(table))]
out <- list(segment = segment,
data = as.matrix(table),
nsamples = dim(table)[1],
nspecies = dim(table)[2])
class(out) <- "xcount"
return(out)}

