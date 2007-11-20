`fill.count` <-
function(table){
out <- table
for(col in 1:ncol(table)){
for(row in 1:nrow(table)){
if(is.na(table[row,col]) == TRUE) out[row,col] <- out[row-1,col]
}}
return(out)}

