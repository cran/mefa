`strify` <-
function(xc, strata, which=c("samples", "species")){
if(is.element(class(xc), c("xcount", "mefa")) == FALSE) 
stop("Object '",xc,"' is not of 'xcount' or 'mefa' class.")
if(is.element(which, c("samples", "species")) == FALSE) stop("Undefined 'which' tag.")
if(class(xc) == "xcount") {
ifelse(which == "samples", table <- xc$data, table <- t(xc$data))
str <- as.factor(strata)}
if(class(xc) == "mefa") {
ifelse(which == "samples", table <- xc$xcount, table <- t(xc$xcount))
ifelse(which == "samples", str <- as.factor(xc$sample.attr[,strata]), 
str <- as.factor(xc$species.attr[,strata]))}
table.out <- array(data=NA, dim=c(nlevels(str), dim(table)[2]))
for(i in 1:dim(table.out)[2]){table.out[,i] <- aggregate(table[,i], list(str), sum)[[2]]}
colnames(table.out) <- colnames(table)
rownames(table.out) <- levels(str)
ifelse(which == "samples", data <- as.matrix(table.out), data <- as.matrix(t(table.out)))
data <- data[order(as.character(rownames(data))), order(as.character(colnames(data)))]
out <- list(segment = xc$segment,
data = data,
nsamples = dim(data)[1],
nspecies = dim(data)[2])
class(out) <- "xcount"
return(out)}

