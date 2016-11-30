get.probs <-
function(ps,pi,alpha,beta,model){
	if(model ==1){
		x = 0:2;
	}else if (model ==0) {
		x = c(0,0,1);
	}else{
		x = c(0,1,1);
	}
	prob.case = ps/pi*exp(alpha+ x*beta)/(1+exp(alpha+ x*beta));
	prob.control = ps/((1-pi)*(1+exp(alpha+ x*beta)));
	list(prob.case = prob.case,prob.control = prob.control)
}
