GFcom.estimate <-
function(d,CI.estimation = TRUE,alpha.CI = 0.05,reportOR = FALSE){
	C = d$pars$C;
	est =d$est;
	beta1 = est[,'beta1'];se1 = est[,'se1'];
	beta2 = est[,'beta2'];se2 = est[,'se2'];
	beta.com = est[,'beta.com'];se.com = est[,'se.com'];
	cMLE= cMLE.se= rep(NA,nrow(est));
	if(CI.estimation){
		CI.cMLE= matrix(NA,nrow = nrow(est),ncol =2);	
	}
	for (i in 1:nrow(est)){
		tmp=MLE.beta.com(beta.com[i],se1[i],se2[i],C,se.com[i]);	
		cMLE[i] = tmp[1];cMLE.se[i] = tmp[2];
		if(CI.estimation){		
			CI.cMLE[i,] = CI.cMLE.beta.com(beta.com[i],se1[i],se2[i],C,se.com[i],alpha.CI,cMLE[i]);	
		}
	}
	MSE.stage1 = cMLE.se^2 + (cMLE-beta2)^2;
	shrink = se2^2/(MSE.stage1+se2^2);
	point.est = shrink*cMLE+(1-shrink)*beta2;
	if(!is.null(rownames(est))){
		names(point.est)= rownames(est);
	}
	d$point.est = point.est;
	if(reportOR){
		d$point.est.OR = round(exp(point.est),2);
	}
	if(CI.estimation){
		z=qnorm(1-alpha.CI/2);
		CI.beta2 = cbind(beta2-se2*z,beta2+se2*z);
		
		beta.lower = shrink*CI.cMLE[,1]+(1-shrink)*CI.beta2[,1];
		beta.upper = shrink*CI.cMLE[,2]+(1-shrink)*CI.beta2[,2];
		CI.est = cbind(beta.lower,beta.upper);
		if(!is.null(rownames(est))){
			rownames(CI.est)= rownames(est);
		}
		d$CI.est = CI.est;
		if(reportOR){
			CI.est.OR = round(exp(CI.est),2);
			colnames(CI.est.OR) = c('OR.L','OR.U');
			d$CI.est.OR = CI.est.OR;
		}
	}
	return(d);
}
