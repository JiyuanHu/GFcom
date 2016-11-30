data.preprocessing <-
function(dat= NULL,genotypes.of.2stages=NULL,model = 1,psudo = 1.1){
	if(!is.null(dat)){
		est1 = recover.beta.se(dat[,c('OR1','OR1.L','OR1.U')]);
		est2 = recover.beta.se(dat[,c('OR2','OR2.L','OR2.U')]);
		est = data.frame(est1,est2);
		names(est)= c('beta1','se1','Z1','p1','beta2','se2','Z2','p2');
		if(!is.null(rownames(dat))){
			rownames(est) = rownames(dat)
		}
		est = est[order(est$p1),]
		alpha = max(est$p1)*psudo;
		C = qnorm(alpha/2,lower.tail = FALSE)	
		pars = list(alpha = alpha,C = C,K = nrow(est))
		d = list(pars = pars);
	}else if (!is.null(genotypes.of.2stages)){	
		est1 = fit.logistic(genotypes.of.2stages[,1:3],genotypes.of.2stages[,4:6],model);
		est2 = fit.logistic(genotypes.of.2stages[,7:9],genotypes.of.2stages[,10:12],model);
		est = data.frame(est1,est2);
		names(est)= c('beta1','se1','Z1','p1','beta2','se2','Z2','p2');
		if(!is.null(rownames(genotypes.of.2stages))){
			rownames(est) = rownames(genotypes.of.2stages)
		}
		o = order(est$p1);
		est = est[o,]
		alpha = max(est$p1)*psudo;
		C = qnorm(alpha/2,lower.tail = FALSE)	
		pars = list(alpha = alpha,C = C,K = nrow(est))
		d = list(pars = pars,genotypes.of.2stages = genotypes.of.2stages[o,]);
	}else{
		stop("You should input dat or genotypes.of.2stages!")      
	}
	est = combined.stat(est);
	d$est = est;	
	return (d)
}
