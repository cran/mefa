`ttsscount` <-
function(table, species.columns=TRUE, segment="unspecified"){
#if(!is.integer(as.integer(as.matrix(table)))) stop("Count must be integer!")
if(species.columns == FALSE) table <- t(table)
zc <- "NULL"
excl <- subset(table, apply(table, 1, sum) ==0)
incl <- subset(table, apply(table, 1, sum) > 0)
incl <- t(incl)
incl <- incl[,order(colnames(incl))]
row.exp <- sum(incl > 0) + dim(excl)[1]
expanded <- array(data = NA, dim = c(row.exp, 4), dimnames = NULL)
r <- 1
for(i in 1:dim(incl)[2]){
incl.c <- incl[,i]
incl.c <- incl.c[order(as.vector(rownames(incl)))]
incl.sub <- subset(incl.c, incl.c > 0)
rownam.sub <- names(incl.sub)
for(j in 1:length(incl.sub)){
expanded[r,1] <- colnames(incl)[i]
expanded[r,2] <- rownam.sub[j]
expanded[r,3] <- segment
expanded[r,4] <- as.numeric(incl.sub[j])
r <- r + 1}}
if(dim(excl)[1] > 0) {excl <- excl[order(rownames(excl)),]
excl.out <- array(data = NA, dim = c(dim(excl)[1], 4), dimnames = NULL)
zc <- "zero.count"
for(z in 1:dim(excl)[1]){
excl.out[z,1] <- rownames(excl)[z]
excl.out[z,2] <- "zero.count"
excl.out[z,3] <- segment
excl.out[z,4] <- 1}
cat(dim(excl)[1], "rows with 0 total count were found and flagged as 'zero.count'.\n")
start <- dim(expanded)[1] - dim(excl.out)[1] + 1
stop <- dim(expanded)[1]
expanded[start:stop,] <- excl.out}
data.out <- data.frame(
as.factor(expanded[,1]),
as.factor(expanded[,2]),
as.factor(expanded[,3]),
as.numeric(expanded[,4]))
colnames(data.out) <- c("sample", "species", "segment", "count")
data.out[] <- lapply(data.out, function(x) x[drop=TRUE])
ifelse (zc == "NULL", nspecies <- nlevels(data.out[,2]), nspecies <- nlevels(data.out[,2]) - 1)
out <- list(data = data.out,
zc = zc,
nsamples = as.numeric(nlevels(data.out[,1])),
nspecies = as.numeric(nspecies),
segment.levels = levels(as.factor(data.out[,3])))
class(out) <- "sscount"
return(out)}

