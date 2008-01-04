`sscount` <-
function(sstable, zc=NULL, fill=FALSE){
if(ncol(as.data.frame(sstable)) < 3 | ncol(as.data.frame(sstable)) > 4) 
stop("Three or four columns are required.")
if(is.null(zc) == FALSE) if(sum(is.element(sstable[,2], zc)) == 0) {
cat("Samples with '", zc, "' were not detected, 'zc' is set to 'NULL'.\n")
zc <- NULL}
if(fill==TRUE) sstable <- fill.count(sstable)
if(sum(is.na(sstable)) != 0) stop("NA values were detected")
sample <- as.factor(sstable[,1])
species <- as.factor(sstable[,2])
if(ncol(sstable) == 3) {
if(!is.numeric(sstable[,3])) stop("Count must be numeric!")
segment <- as.factor(rep("undefined", nrow(sstable)))
count <- as.numeric(sstable[,3])}

if(ncol(sstable) == 4) {
if(!is.numeric(sstable[,4])) stop("Count must be numeric!")
segment <- as.factor(sstable[,3])
count <- as.numeric(sstable[,4])}

frame <- data.frame(sample, species, segment, count)
frame[] <- lapply(frame, function(x) x[drop=TRUE])
colnames(frame) <- c("sample", "species", "segment", "count")
if(is.null(zc)) {nspecies <- nlevels(frame[,2])} else {nspecies <- nlevels(frame[,2]) - 1}
out <- list(data = frame, 
zc = zc,
nsamples = as.numeric(nlevels(frame[,1])),
nspecies = as.numeric(nspecies),
segment.levels = levels(frame[,3]))
class(out) <- "sscount"
return(out)}

