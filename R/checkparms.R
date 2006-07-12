"checkparms" <-
function(Kernel) {
if (! is.function(Kernel)) {
stop('Argument Kernel is not an existing function')
}
if (integrate(Kernel,-Inf,Inf,stop.on.error=FALSE)$message!="OK") {
stop('improper Kernel')
}
if (! identical(all.equal(integrate(Kernel,-Inf,Inf,stop.on.error=FALSE)$value,1),TRUE)) {
stop('improper Kernel')
}
}

