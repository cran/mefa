`xcount` <-
function (ssc, segment = 0) 
{
    if (class(ssc) != "sscount") 
        stop("Object '", ssc, "' is not of 'sscount' class.")
    infl <- inflate(ssc$data[, 1:3], ssc$data[, 4])
    if (segment == 0 | segment == "all") {
        crosstable <- as.array(table(infl[, 2], infl[, 1]))
        segment.index <- "all"
    }
    else {
        crosstable <- as.array(table(infl[, 2], infl[, 1], infl[, 
            3]))[, , segment]
if(is.character(segment)) segment <- which(ssc$segment.levels == segment)
        segment.index <- ssc$segment.levels[segment]
    }
    rct <- rownames(crosstable)
    cct <- colnames(crosstable)
    crosstable.sub <- subset(crosstable, row.names(crosstable) != 
        ssc$zc)
    out.data <- matrix(data = crosstable.sub, nrow = ssc$nspecies, 
        ncol = ssc$nsamples)
    if (ssc$zc == "NULL") {
        rownames(out.data) <- rct
    }
    else {
        rownames(out.data) <- rct[1:length(rct) - 1]
        cat("Samples with '", ssc$zc, "' were detected.\n")
    }
    colnames(out.data) <- cct
    out <- list(segment = segment.index, data = as.matrix(t(out.data)), 
        nsamples = dim(out.data)[2], nspecies = dim(out.data)[1])
    class(out) <- "xcount"
    return(out)
}
