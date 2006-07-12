"biweight" <-
function(x) {

ifelse(abs(x)<=1,15/16*(1-x^2)^2,0)
}

