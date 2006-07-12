"mkle" <-
function(data,Kernel=dnorm,bw=2*sd(data),small=TRUE) {

checkparms(Kernel) 

# control$fnscale = negative changes it to maximization!

if (small) {
optim(mean(data),klik,method="BFGS",control=list(fnscale=-1),data=data,bw=bw,Kernel=Kernel)$par
}else{
optim(mean(data),klik,method="BFGS",control=list(fnscale=-1),data=data,bw=bw,Kernel=Kernel)
}


}

