get.probs.rank <-
function(ps,pi,alphas,betas,model){
	if(model ==1){
		score = 0:2
	}else if (model ==0){
		score = c(0,0,1)
	}else{
		score = c(0,1,1)
	}	
	prob.case = ps/pi*cbind(exp(alphas)/(1+exp(alphas)),exp(alphas+score[2]*betas)/(1+exp(alphas+score[2]*betas)),
		exp(alphas+score[3]*betas)/(1+exp(alphas+score[3]*betas)));
	prob.control = ps/(1-pi)/cbind(1+exp(alphas),1+exp(alphas+score[2]*betas),1+exp(alphas+score[3]*betas));
	list(prob.case = prob.case,prob.control = prob.control)
}
