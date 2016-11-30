combined.stat <-
function(est){
	beta1 = est[,'beta1'];se1 = est[,'se1'];
	beta2 = est[,'beta2'];se2 = est[,'se2'];
	est$beta.com = se2^2/(se1^2+se2^2)*beta1 + se1^2/(se1^2+se2^2)*beta2;
	est$se.com = se1*se2/sqrt(se1^2+se2^2);
	return(est);
}
