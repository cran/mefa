`exclmf` <-
function(xc, which=c("samples", "species"), excl, empty=TRUE){
if (is.element(class(xc), c("xcount", "mefa")) == FALSE)
stop("Object '", xc, "' is not of 'xcount' or 'mefa' class.")
if(class(xc) == "xcount") matr <- xc$data
if(class(xc) == "mefa") matr <- xc$xcount
if(which == "species") matr <- t(matr)
if(empty == TRUE) if(nrow(matr) - length(excl) <= 1) stop("'excl' list is too long.")
if(is.null(excl)) {matr.ex <- matr} else {
if(is.character(excl)) excl <- which(rownames(matr) == excl)
rok <- is.element(which(rownames(matr) == rownames(matr)), excl)
matr.ex <- subset(matr, rok != TRUE)
}
if(which == "species") matr.ex <- t(matr.ex)
if(empty == FALSE) {
if(nrow(matr.ex) - length(excl) <= 1) stop("'excl' list is too long.")
matr.ex <- subset(matr.ex, apply(matr.ex, 1, sum) > 0)
}
if(class(xc) == "xcount") out <- as.xcount(matr.ex, segment = xc$segment)
if(class(xc) == "mefa") {
xc.out <- as.xcount(matr.ex, segment = xc$segment)
out <- mefa(xc.out,
xorder(xc.out, "samples", xc$sample.attr),
xorder(xc.out, "species", xc$species.attr))
}
return(out)
}

