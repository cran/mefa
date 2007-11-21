`msscount` <-
function(ssc1, ssc2){
if(class(ssc1) != "sscount" | class(ssc2) != "sscount") stop("Objects must be of 'sscount' class.")
zzc <- "NULL"
if(ssc1$zc != "NULL" & ssc2$zc != "NULL")
{if(ssc1$zc != ssc2$zc) cat("Different 'zero.count' indicators were united.\n")
if(sum(is.element(subset(ssc1$data$species, ssc1$data$species != ssc1$zc), ssc2$zc)) != 0)
stop("Zero count identifier was detected for not zero count sample!")
if(sum(is.element(subset(ssc2$data$species, ssc2$data$species != ssc2$zc), ssc1$zc)) != 0)
stop("Zero count identifier was detected for not zero count sample!")}
if(ssc1$zc == "NULL") {ssc1.sub <- ssc1$data} else {
ssc1.sub <- subset(ssc1$data, ssc1$data$species != ssc1$zc)}
if(ssc2$zc == "NULL") {ssc2.sub <- ssc2$data} else {
ssc2.sub <- subset(ssc2$data, ssc2$data$species != ssc2$zc)}
ssc1.zc <- subset(ssc1$data[,1], ssc1$data$species == ssc1$zc)
ssc2.zc <- subset(ssc2$data[,1], ssc2$data$species == ssc2$zc)
rows.sub <- dim(ssc1.sub)[1] + dim(ssc2.sub)[1]
rows.zc <- length(ssc1.zc) + length(ssc2.zc)
rows.out <- rows.sub + rows.zc
start2 <- dim(ssc1.sub)[1] + 1
start.zc1 <- rows.sub + 1
stop.zc1 <- rows.sub + length(ssc1.zc)
start.zc2 <- start.zc1 + length(ssc1.zc)
data.out <- as.data.frame(array(data = NA, dim = c(rows.out, 4), dimnames = NULL))
data.out[1:dim(ssc1.sub)[1],1] <- as.character(ssc1.sub[,1])
data.out[1:dim(ssc1.sub)[1],2] <- as.character(ssc1.sub[,2])
data.out[1:dim(ssc1.sub)[1],3] <- as.character(ssc1.sub[,3])
data.out[1:dim(ssc1.sub)[1],4] <- as.numeric(ssc1.sub[,4])
data.out[start2:rows.sub,1] <- as.character(ssc2.sub[,1])
data.out[start2:rows.sub,2] <- as.character(ssc2.sub[,2])
data.out[start2:rows.sub,3] <- as.character(ssc2.sub[,3])
data.out[start2:rows.sub,4] <- as.numeric(ssc2.sub[,4])
if(ssc1$zc != "NULL"){
data.out[start.zc1:stop.zc1,1] <- as.character(ssc1.zc)
data.out[start.zc1:stop.zc1,2] <- as.character("zero.count")
data.out[start.zc1:stop.zc1,3] <- as.character("unspecified")
data.out[start.zc1:stop.zc1,4] <- as.numeric(1)
zzc <- "zero.count"}
if(ssc2$zc != "NULL"){
data.out[start.zc2:rows.out,1] <- as.character(ssc2.zc)
data.out[start.zc2:rows.out,2] <- as.character("zero.count")
data.out[start.zc2:rows.out,3] <- as.character("unspecified")
data.out[start.zc2:rows.out,4] <- as.numeric(1)
zzc <- "zero.count"}
data.fin <- data.frame(
as.factor(data.out[,1]),
as.factor(data.out[,2]),
as.factor(data.out[,3]),
as.numeric(data.out[,4]))
colnames(data.fin) <- c("sample", "species", "segment", "count")
data.fin[] <- lapply(data.fin, function(x) x[drop=TRUE])
ifelse (ssc1$zc == "NULL" , nspecies <- nlevels(data.fin[,2]), nspecies <- nlevels(data.fin[,2]) - 1)
out <- list(data = as.data.frame(data.fin), 
zc = zzc,
nsamples = as.numeric(nlevels(data.fin[,1])),
nspecies = as.numeric(nspecies),
segment.levels = levels(data.fin[,3]))
class(out) <- "sscount"
return(out)}

