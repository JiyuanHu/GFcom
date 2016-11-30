generate.multinomial <-
function(size,prob){
	t(mapply(rmultinom,prob = unclass(as.data.frame(t(prob))),MoreArgs = list(n = 1,size = size)));
}
