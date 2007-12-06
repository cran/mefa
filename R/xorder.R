`xorder` <-
function(xc, which=c("samples", "species"), attrib, index=1){
if(class(xc) != "xcount") stop("Object '",xc,"' is not of 'xcount' class.")
check1 <- check.attrib(xc, which, attrib, index)$set.relation
check2 <- check.attrib(xc, which, attrib, index)$duplicate
if(sum(is.element(c("equal", "inclusion"), check1)) == 0) stop("Set relation in 'check.attrib' result: ", 
check1, ".\nMissing elements: ", check.attrib(xc, which, attrib, index)$missing)
if(check2 != "NULL") stop("Duplicates were found by 'check.attrib': ", check2, ".")
if(check1 == "equal"){
attrib.out <- attrib[order(as.character(attrib[,index])),]
if(which == "species") index.out <- as.character(colnames(xc$data))
if(which == "samples") index.out <- as.character(rownames(xc$data))}
if(check1 == "inclusion"){
if(which == "species") {
exclude <- is.element(attrib[,index],colnames(xc$data))
index.out <- as.character(colnames(xc$data))}
if(which == "samples") {
exclude <- is.element(attrib[,index],rownames(xc$data))
index.out <- as.character(rownames(xc$data))}
attrib.sub <- subset(attrib, exclude == TRUE)
attrib.sub[] <- lapply(attrib.sub, function(x) x[drop=TRUE])
attrib.out <- attrib.sub[order(as.character(attrib.sub[,index])),]}
rownames(attrib.out) <- index.out
attrib.out <- as.data.frame(attrib.out)
out <- list(which = which, check.setrel = check1, check.dupl = check2, data = attrib.out)
class(out) <- "xorder"
return(out)}

