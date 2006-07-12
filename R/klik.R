"klik" <-
function(theta=0,data,Kernel=dnorm,bw=2*sd(data)) {

sum(log(Kdensity(data,data,Kernel,bw,theta)))

}

