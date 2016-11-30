recover.beta.se <-
function(ORs){
	z = qnorm(1-0.05/2);
	betas = log(ORs);
	beta = betas[,1];
	se = (betas[,3]-betas[,2])/(2*z);
	z.wald = beta/se;
	p.wald = pnorm(abs(z.wald),lower.tail = FALSE)*2;
	est = cbind(beta,se,z.wald,p.wald);
	return(est);
}
