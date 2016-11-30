followup.stage.simulation <-
function(d,n.cases2,n.controls2,model){	
	cases2 = generate.multinomial(n.cases2,d$prob.case);
	controls2 = generate.multinomial(n.controls2,d$prob.control);
	est2 = fit.logistic(cases2,controls2,model)[,1:2];
	colnames(est2) = c('beta2','se2');
	beta1 = d$est1[,1];se1 = d$est1[,2];
	beta2 = est2[,1];se2 = est2[,2];
	beta.com = se2^2/(se1^2+se2^2)*beta1 + se1^2/(se1^2+se2^2)*beta2;
	se.com = se1*se2/sqrt(se1^2+se2^2);
	est = cbind(beta1,se1,beta2,se2,beta.com,se.com);
	name = c('beta1','se1','beta2','se2','beta.com','se.com');	
	genotypes.of.2stages = cbind(d$cases1,d$controls1,cases2,controls2);
	colnames(genotypes.of.2stages) = c('DAA1','DAa1','Daa1','NAA1','NAa1','Naa1','DAA2','DAa2','Daa2','NAA2','NAa2','Naa2');	
	d = list(pars = d$pars,est = data.frame(est),genotypes.of.2stages = data.frame(genotypes.of.2stages));
	return(d);
}
