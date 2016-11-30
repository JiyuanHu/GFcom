first.stage.simulation <-
function(n.cases1,n.controls1,pi,I,M,model){
	pars = list(n.cases1= n.cases1, n.controls1 = n.controls1,pi = pi,I = I,M = M,C0=C0,model = model);
	p = runif(M,0.02,0.5);	
	ps = cbind((1-p)^2,2*p*(1-p),p^2);#Assume HWE
	beta.p1 = abs(rnorm(I,0,sd = sqrt(0.02)));	
	beta.p2 = rep(0,M-I);
	betas = c(beta.p1,beta.p2);
	alphas0 = mapply(alpha.est,ps=unclass(as.data.frame(t(ps))),beta = betas,MoreArgs = list(pi = pi,model = model))	
	probs = get.probs.rank(ps,pi,alphas0,betas,model);	
	cases1 = generate.multinomial(n.cases1,probs$prob.case);
	controls1 = generate.multinomial(n.controls1,probs$prob.control);
	dat = list(pars = pars,parameters = cbind(p,betas,alphas0),probs = probs,cases1=cases1,controls1= controls1);
	return(dat);
}
