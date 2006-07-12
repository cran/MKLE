"Kdensity" <-
function (x,data,Kernel=dnorm,bw=2*sd(data),theta=mean(data)) 
{

datamat<-matrix(rep(data,length(x)),ncol=length(data),byrow=TRUE)
1/(length(data)*bw)*apply(Kernel((x-datamat-mean(data)+theta)/bw),1,sum)

}

