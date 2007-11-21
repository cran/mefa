`mxcount` <-
function(xc1,xc2,segment="unspecified"){
out <- xcount(msscount(ttsscount(as.data.frame(xc1$data)), ttsscount(as.data.frame(xc2$data))))
out$segment <- segment
return(out)}

