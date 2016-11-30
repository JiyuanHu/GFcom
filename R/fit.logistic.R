fit.logistic <-
function(cases,controls,model){
	genotypes = cbind(cases,controls);
	res = matrix(NA,nrow = nrow(genotypes),ncol = 4);
	for (i in 1:nrow(genotypes)){
		tmp = genotypes[i,];	
		dat = transform.genotypes(tmp,model);
		fit = glm(disease~genotype,family=binomial(link="logit"),data = dat);
		stat= summary(fit)$coefficients[2,];		
		res[i,]=stat;
		if(i==1){
		colnames(res) = names(stat);
		}
	}
	return(res)
}
