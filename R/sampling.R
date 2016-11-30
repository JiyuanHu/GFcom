sampling <-
function(n.cases,n.controls,probs,rep.No=1e5){
	prob.case = probs$prob.case;
	prob.control = probs$prob.control;
	cases =  t(rmultinom(rep.No,n.cases,prob.case));
	controls = t(rmultinom(rep.No,n.controls,prob.control));
	sums = cases+controls;	
	res = list(cases = cases,controls = controls,sums = sums);
	return(res);	
}
