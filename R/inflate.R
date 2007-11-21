`inflate` <-
function(factors, count){
if(nrow(factors) != length(count)) stop("Number of dimensions must be equal!")
factors <- data.frame(factors)
n.total <- sum(count)
row.orig <- nrow(factors)
col.orig <- ncol(factors)
infl.data <- array(data=NA, dim=c(n.total, col.orig))
colnames(infl.data) <- colnames(factors)
j <- 1
for (i in 1:row.orig){
for(individuals in 1:count[i]){
if (count[i] < 0)  stop("Negative count value.") else {
if (count[i] == 0) stop("Zero count value.") else {
infl.data[j, 1:col.orig] <- as.matrix(factors[i, 1:col.orig])
j<-j+1}}}}
return(data.frame(infl.data))}

