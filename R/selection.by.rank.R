selection.by.rank <-
function(dat,C0,K,model,psudo){
	pars = dat$pars;
	n.cases1 = pars$n.cases1;
	n.controls1 = pars$n.controls1;
	model = pars$model;
	cases1=dat$cases1;
	controls1= dat$controls1;	
	T = CRtest(cases1,controls1,n.cases1,n.controls1,model)
	ind = which(T>C0);
	if(length(ind)<K){
		ind = which(T>C0*.8);
	}
	res = fit.logistic(cases1[ind,],controls1[ind,],model );
	o = order(abs(res[,3]),decreasing = TRUE)[1:K]
	res = res[o,]
	est1 = res[,1:2];
	colnames(est1) = c('beta1','se1');
	alpha = psudo*max(res[,4]);
	C = qnorm(alpha/2,lower.tail = FALSE)	
	ind2 = ind[o]; 
	parameters = dat$parameters[ind2,];
	pars$beta = parameters[,2];
	pars$p =parameters[,1];
	pars$alpha0 = parameters[,3];
	pars$alpha = alpha;
	pars$C = C;
	pars$K = K;
	d = list(pars = pars,
		prob.case = dat$probs$prob.case[ind2,],
		prob.control = dat$probs$prob.control[ind2,],
		cases1 = dat$cases1[ind2,],
		controls1 = dat$controls1[ind2,],
		est1 = est1
		)
	return (d);
}
