"triweight" <-
function(x) {

ifelse(abs(x)<=1,35/32*(1-x^2)^3,0)

}

