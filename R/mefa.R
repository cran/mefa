`mefa` <-
function(xc, xorder.samples, xorder.species){
if(class(xc) != "xcount") stop("Object '",xc,"' is not of 'xcount' class.")
if(class(xorder.species) != "xorder") stop("Object '",xorder.species,"' is not of 'xorder' class.")
if(xorder.species$which != "species") stop("Species/sample mismatch.")
rspec <- rownames(xorder.species$data)
if(class(xorder.samples) != "xorder") stop("Object '",xorder.samples,"' is not of 'xorder' class.")
if(xorder.samples$which != "samples") stop("Species/sample mismatch.")
rsamp <- rownames(xorder.samples$data)
cxc <- colnames(xc$data)
rxc <- rownames(xc$data)
if(length(rspec) != length(cxc)) stop("Species list lengths differ.")
if(length(rsamp) != length(rxc)) stop("Sample list lengths differ.")
if(sum(rspec != cxc) != 0) stop("Species list mismatch.")
if(sum(rsamp != rxc) != 0) stop("Sample list mismatch.")
out <- list(segment = xc$segment,
xcount = as.matrix(xc$data),
sample.attr = as.data.frame(xorder.samples$data),
species.attr = as.data.frame(xorder.species$data),
nsamples = xc$nsamples,
nspecies = xc$nspecies,
totalcount = sum(xc$data),
nsample.attr = length(colnames(xorder.samples$data)),
nspecies.attr = length(colnames(xorder.species$data)))
class(out) <- "mefa"
return(out)}

