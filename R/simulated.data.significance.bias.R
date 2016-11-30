simulated.data.significance.bias <-
function(n.cases1,n.controls1,n.cases2,n.controls2,beta= NULL,OR = NULL,p,pi,C=NULL,alpha = NULL,M=100,model=1 ){	
	if(is.null(C) & is.null(alpha)){
      stop("You should either set threshold C or significance level alpha.")      
    }else if(!is.null(C)){
		alpha = pnorm(C,lower.tail = FALSE)*2;
	}else if(!is.null (alpha)){
		C = -qnorm(alpha/2);
	}
	if(is.null(beta) & is.null(OR)){
      stop("You should either give log OR beta or OR.")      
    }else if(!is.null(beta)){
		OR = round(exp(beta),1);
	}else if(!is.null (OR)){
		beta = log(OR);
	}		
	ps = c((1-p)^2,2*p*(1-p),p^2);
	alpha0 = alpha.est(ps,beta,pi,model);
	res = repetition(n.cases1,n.controls1,n.cases2,n.controls2,ps,alpha,beta,pi,C,model,M); 	
	pars = list(n.cases1=n.cases1,n.controls1=n.controls1,n.cases2= n.cases2,n.controls2 =n.controls2,
		beta = beta,OR = OR,p = p,pi = pi,C = C,alpha =alpha,M = M,model = model,alpha0 = alpha0,ps =ps);	
	d = list(pars = pars,est= res$est,genotypes.of.2stages = res$genotypes.of.2stages);
	rm(res);gc();
	d;
}
