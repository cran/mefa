`mxcount` <-
function(xc1,xc2,segment="unspecified"){
out <- xcount(msscount(ttsscount(as.data.frame(xc1$data)), ttsscount(as.data.frame(xc2$data))))
if(xc1$segment == xc2$segment) {out$segment <- xc1$segment} else {out$segment <- segment}
return(out)}

