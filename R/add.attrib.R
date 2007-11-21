`add.attrib` <-
function(mf, which=c("samples", "species"), attrib, index=1){
if(class(mf) != "mefa") stop("Object '",mf,"' is not of 'mefa' class.")
xord <- xorder(as.xcount(mf$xcount), which, attrib, index)
xord$data[[index]] <- NULL
if(which == "species") {
xsamp.out <- mf$sample.attr
xspec.out <- data.frame(mf$species.attr, xord$data)
nsample.attr <- mf$nsample.attr
nspecies.attr <- dim(xspec.out)[2]}
if(which == "samples") {
xsamp.out <- data.frame(mf$sample.attr, xord$data)
xspec.out <- mf$species.attr
nsample.attr <- dim(xsamp.out)[2]
nspecies.attr <- mf$nspecies.attr}
out <- list(segment = mf$segment,
xcount = as.matrix(mf$xcount),
sample.attr = as.data.frame(xsamp.out),
species.attr = as.data.frame(xspec.out),
nsamples = mf$nsamples,
nspecies = mf$nspecies,
totalcount = mf$totalcount,
nsample.attr = nsample.attr,
nspecies.attr = nspecies.attr)
class(out) <- "mefa"
return(out)}

