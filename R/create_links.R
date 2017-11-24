create_links <-
function(q,q1,f,f1,m,d,l){
for(i in 1 : length(f)){
s <- rep(0,length(f1))
for(j in 1:length(q[[i]])){
for(k in 1:length(f1)){
if(q[[i]][j] %in% q1[[k]] & s[k]==0){
s[k] <- 1
segments3d(rbind(c(f[i],0,0), # start
c(m+f1[k],d,l)), color = "grey" # 
)
}}
}
}	
}
