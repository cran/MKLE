"mklewarp" <-
function(data,bw=1,gs=2^11,K="gaussian"){
  klikwarp<-function(theta,data,kde,grid,min){
    dshift<-data-mean(data)+theta
    vec<-(dshift-min)/grid
    index<-round(vec)+1
    sum(log(kde$y[index]+(kde$y[index+1]-kde$y[index])/
    grid*(dshift-kde$x[index])))
}

range<-range(data)
if(K=="gaussian"){
  kde<-density(data,bw=bw,from=range[1]-3*bw,
  to=range[2]+3*bw,n=gs,kernel=K)
}else{
  kde<-density(data,width=2*bw,from=range[1]-3*bw,
  to=range[2]+3*bw,n=gs,kernel=K)
}
min<-kde$x[1]
grid<-kde$x[2]-min
optimize(klikwarp,maximum=TRUE,l=mean(data)-2*bw,u=mean(data)+2*bw,
data=data,kde=kde,grid=grid,min=min)$maximum
}

