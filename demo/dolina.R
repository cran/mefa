cat("This is the demo for the R package 'mefa'.\n\n")

library(mefa)
data(dol.count, dol.sample, landsnail)

wait <- function() {
input <- readline("Please press ENTER to contimue... ")
}

ssc <- sscount(fill.count(dol.count), zc="zero.count")
xc.broken <- xcount(ssc, 2)
dmf <- mefa(
dxc <- xcount(ssc),
xorder(dxc, which="samples", dol.sample),
xorder(dxc, which="species", landsnail, 2)
)

# litter transect
cat("\nCount data and sample attributes can be directly used for plotting.\n\n")
wait()
par(mfrow=c(1,2), pty="s")
plot(apply(subset(dmf$xcount, dmf$sample.attr$microhabitat == "litter"), 1, sum)
    ~ subset(dmf$sample.attr[,3], dmf$sample.attr$microhabitat == "litter"),
    ylab="Abundance", xlab="Transect position", type="b", col="red",
    main="Litter transect along the dolina", sub="1-7: south to north aspect")
plot(apply(subset(dmf$xcount, dmf$sample.attr$microhabitat == "litter") > 0, 1, sum)
    ~ subset(dmf$sample.attr[,3], dmf$sample.attr$microhabitat == "litter"),
    ylab="Species richness", xlab="Transect position", type="b", col="red",
    sub="1-7: south to north aspect")
par(mfrow=c(1,1))

# linear model
cat("\nCount data and sample attributes can be directly used for \nunivariate modelling.\n\n")
wait()
print(
    summary(lm(apply(dmf$xcount > 0, 1, sum) ~ dmf$sample.attr$microhabitat -1))
    )

# boxplot
wait()
boxplot(
    apply(dmf$xcount > 0, 1, sum)
    ~ dmf$sample.attr$microhabitat,
    main="", ylab="Species Richness", xlab="Microhabitats", col="red")

# cluster
cat("\nCount data and sample attributes can be directly used for \nmultivariate analyses.\n\n")
wait()
plot(
    hclust(dist(dmf$xcount, "euclidean"), "ward"),
    sub="Euclidean distance, Ward's method", main="Cluster dendrogram of abundance data",xlab="")

# taxonomy
cat("\nCount data, sample and species attributes can be directly used \nin contingency tables.\n\n")
wait()
tax <- strify(strify(dmf, "microhabitat", "samples"), dmf$species.attr$familia, "species")$data
print(tax<-tax[,c("Clausiliidae","Zonitidae","Helicidae")])
print(chisq.test(tax, simulate.p.value = TRUE, B = 2000))

# segments
cat("\nSegments can be directly used to \nevaluate effects of subsetting.\n\n")
wait()
tax.broken <- strify(strify(xc.broken, dmf$sample.attr$microhabitat, "samples"),
    dmf$species.attr$familia, "species")$data
tax.broken<-tax.broken[,c("Clausiliidae","Zonitidae","Helicidae")]
tax.m <- array(data=c(tax.broken, tax - tax.broken), dim=c(nrow(tax), ncol(tax), 2))
dimnames(tax.m)<-list(rownames(tax), colnames(tax), c("Broken","Intact"))

par(mfrow=c(1,2), pty="s")
barplot(t(tax.m[,,1]),horiz = TRUE,col=heat.colors(3),
    main="Broken", xlab="Frequency")
barplot(t(tax.m[,,2]),horiz = TRUE,col=heat.colors(3),
    main="Intact", xlab="Frequency", legend.text=TRUE)
par(mfrow=c(1,1))

cat("\nEnd of 'mefa' demo.\n\n")
